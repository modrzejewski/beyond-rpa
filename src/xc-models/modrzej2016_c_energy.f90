module modrzej2016_c_energy
      use arithmetic
      use math_constants
      use tpss_xc_auxiliary
      use modrzej2012_c_energy

      implicit none

      real(F64), parameter :: D_SCALING = 1.1_F64
      !
      ! Threshold for discarding grid points with small electron density      
      !
      real(F64), parameter :: MODRZEJ2016_C_RHO_THRESH = 1.0E-10_F64
      !
      ! The value of Faa has been optimized by fitting the
      ! parallel-spin component of Vc (lambda = 1) to exact
      ! (Monte Carlo) energies of the homogeneous electron gas.
      !
      real(F64), parameter :: MCSv2_Faa = D_SCALING * 2.642170768239934_F64
      !
      ! The value of Fab has been optimized by fitting the
      ! opposite-spin component of Vc (lambda = 1) to exact
      ! (Monte Carlo) energies of the homogeneous electron gas.
      !
      real(F64), parameter :: MCSv2_Fab = D_SCALING * 2.106996766523905_F64
      !
      ! Coefficient of the gradient term in the correlation-hole
      ! damping factor
      !
      real(F64), parameter :: MCSv2_Gpar = 0.0150_F64
      real(F64), parameter :: MCSv2_Gopp = TWO**(TWO/THREE) * MCSv2_Gpar
      
      real(F64), parameter :: MCSv3_Gpar = D_SCALING * 0.0135_F64
      real(F64), parameter :: MCSv3_Gopp = TWO**(TWO/THREE) * MCSv3_Gpar
      
      real(F64), parameter :: MCSv4_GPARAM = 0.04_F64
      
contains

      subroutine mcsv4_c(eps, ec_rho, ec_sig, ec_tau, rho, sigma, tau)
            real(F64), intent(out) :: eps
            real(F64), intent(out) :: ec_rho
            real(F64), intent(out) :: ec_sig
            real(F64), intent(out) :: ec_tau
            real(F64), intent(in)  :: rho
            real(F64), intent(in)  :: sigma
            real(F64), intent(in)  :: tau
            
            call mlrcs12_c(eps, ec_rho, ec_sig, ec_tau, rho, sigma, tau, MCSv4_GPARAM)
      end subroutine mcsv4_c


      subroutine u_mcsv4_c(eps, ec_rhoa, ec_rhob, ec_sigaa, ec_sigab, ec_sigbb, ec_taua, &
            ec_taub, rhoa, rhob, sigaa, sigab, sigbb, taua, taub)
            real(F64), intent(out) :: eps
            real(F64), intent(out) :: ec_rhoa
            real(F64), intent(out) :: ec_rhob
            real(F64), intent(out) :: ec_sigaa
            real(F64), intent(out) :: ec_sigab
            real(F64), intent(out) :: ec_sigbb
            real(F64), intent(out) :: ec_taua
            real(F64), intent(out) :: ec_taub
            real(F64), intent(in)  :: rhoa
            real(F64), intent(in)  :: rhob
            real(F64), intent(in)  :: sigaa
            real(F64), intent(in)  :: sigab
            real(F64), intent(in)  :: sigbb
            real(F64), intent(in)  :: taua
            real(F64), intent(in)  :: taub
            
            call u_mlrcs12_c(eps, ec_rhoa, ec_rhob, ec_sigaa, ec_sigab, ec_sigbb, ec_taua, ec_taub, &
                  rhoa, rhob, sigaa, sigab, sigbb, taua, taub, MCSv4_GPARAM)
      end subroutine u_mcsv4_c

      
      subroutine modrzej2016_acd_c(epsc, ec_rho, ec_sigma, ec_tau, rho, sigma, tau)
            !
            ! Compute the correlation energy of Modrzejewski et al. using the AC-D model
            ! of the adiabatic connection integrand.
            !
            ! This subroutine is for spin-compensated densities. This subroutine
            ! was checked with Mathematica.
            !
            ! 1. Modrzejewski et al. unpublished
            ! 2. Teale, A.M., Coriani, S., Helgaker, T. J. Chem. Phys. 132, 164115 (2010);
            !    doi: 10.1063/1.3380834
            ! 3. Modrzejewski et al. J. Chem. Phys. 137, 204121 (2012);
            !    doi: 10.1063/1.4768228
            !
            real(F64), intent(out) :: epsc
            real(F64), intent(out) :: ec_rho
            real(F64), intent(out) :: ec_sigma
            real(F64), intent(out) :: ec_tau
            real(F64), intent(in)  :: rho
            real(F64), intent(in)  :: sigma
            real(F64), intent(in)  :: tau

            real(F64) :: vc0, epsc0, vc0_rho, vc0_sigma, vc0_tau, vcinf_sigma
            real(F64) :: vcinf, epscinf, vcinf_rho
            real(F64) :: vcinf_tau, rho_a, rho_b, sigma_aa, sigma_ab, sigma_bb, tau_a, ec, ec_vcinf, ec_vc0
            real(F64) :: vcab, vcab_rhoa, vcab_rhob, vcab_sigaa, vcab_sigab, vcab_sigbb
            real(F64) :: vcaa, vcaa_rhoa, vcaa_sigaa, vcaa_taua

            if (rho > TWO*MODRZEJ2016_C_RHO_THRESH) then
                  rho_a = rho / TWO
                  rho_b = rho / TWO
                  sigma_aa = sigma / FOUR
                  sigma_ab = sigma / FOUR
                  sigma_bb = sigma / FOUR
                  tau_a = tau / TWO
                  !
                  ! d/dlambda Vc(lambda)|lambda->0 and its derivatives
                  !
                  call vcab_0_deriv(vcab, vcab_rhoa, vcab_rhob, vcab_sigaa, vcab_sigab, vcab_sigbb, &
                        rho_a, rho_b, sigma_aa, sigma_ab, sigma_bb, MCSv2_Gopp, MCSv2_Fab)
                  call vcaa_0_deriv(vcaa, vcaa_rhoa, vcaa_sigaa, vcaa_taua, rho_a, sigma_aa, tau_a, MCSv2_Gpar, MCSv2_Faa)
                  vc0 = TWO * (vcaa + vcab)
                  epsc0 = TWO * (vcaa + vcab) / rho
                  vc0_rho = vcaa_rhoa + vcab_rhoa + vcab_rhob
                  vc0_sigma = (vcaa_sigaa + vcab_sigaa + vcab_sigab + vcab_sigbb) * (ONE/TWO)
                  vc0_tau = vcaa_taua
                  !
                  ! Vc(lambda->Infinity) and its derivatives
                  !
                  call point_charge_c(epscinf, vcinf_rho, rho)
                  vcinf = rho * epscinf
                  vcinf_sigma = ZERO
                  vcinf_tau = ZERO
                  !
                  ! Use the AC-D model at lambda=1 (Eq. 59 in Ref. 2)
                  !
                  epsc = epscinf * epsc0 / (TWO * epscinf + epsc0)
                  ec = rho * epsc
                  ec_vcinf = vc0**2 / (TWO * vcinf + vc0)**2
                  ec_vc0 = TWO * vcinf**2 / (TWO * vcinf + vc0)**2
                  ec_rho = ec_vcinf * vcinf_rho + ec_vc0 * vc0_rho
                  ec_sigma = ec_vcinf * vcinf_sigma + ec_vc0 * vc0_sigma
                  ec_tau = ec_vcinf * vcinf_tau + ec_vc0 * vc0_tau
            else
                  epsc = ZERO
                  ec_rho = ZERO
                  ec_sigma = ZERO
                  ec_tau = ZERO
            end if
      end subroutine modrzej2016_acd_c


      subroutine modrzej2016_acd_c_tpss(epsc, ec_rho, ec_sigma, ec_tau, rho, sigma, tau)
            !
            ! Compute the correlation energy of Modrzejewski et al. using the AC-D model
            ! of the adiabatic connection integrand.
            !
            ! This subroutine is for spin-compensated densities. This subroutine
            ! was checked with Mathematica.
            !
            ! 1. Modrzejewski et al. unpublished
            ! 2. Teale, A.M., Coriani, S., Helgaker, T. J. Chem. Phys. 132, 164115 (2010);
            !    doi: 10.1063/1.3380834
            ! 3. Modrzejewski et al. J. Chem. Phys. 137, 204121 (2012);
            !    doi: 10.1063/1.4768228
            !
            real(F64), intent(out) :: epsc
            real(F64), intent(out) :: ec_rho
            real(F64), intent(out) :: ec_sigma
            real(F64), intent(out) :: ec_tau
            real(F64), intent(in)  :: rho
            real(F64), intent(in)  :: sigma
            real(F64), intent(in)  :: tau

            real(F64) :: vc0, epsc0, vc0_rho, vc0_sigma, vc0_tau, vcinf_sigma
            real(F64) :: vcinf, epscinf, vcinf_rho, vcinf_rhoa, vcinf_rhob
            real(F64) :: vcinf_sigaa, vcinf_sigab, vcinf_sigbb, vcinf_taua, vcinf_taub
            real(F64) :: vcinf_tau, rho_a, rho_b, sigma_aa, sigma_ab, sigma_bb, tau_a, tau_b
            real(F64) :: ec, ec_vcinf, ec_vc0
            real(F64) :: vcab, vcab_rhoa, vcab_rhob, vcab_sigaa, vcab_sigab, vcab_sigbb
            real(F64) :: vcaa, vcaa_rhoa, vcaa_sigaa, vcaa_taua

            if (rho > TWO*MODRZEJ2016_C_RHO_THRESH) then
                  rho_a = rho / TWO
                  rho_b = rho / TWO
                  sigma_aa = sigma / FOUR
                  sigma_ab = sigma / FOUR
                  sigma_bb = sigma / FOUR
                  tau_a = tau / TWO
                  tau_b = tau / TWO
                  !
                  ! d/dlambda Vc(lambda)|lambda->0 and its derivatives
                  !
                  call vcab_0_deriv(vcab, vcab_rhoa, vcab_rhob, vcab_sigaa, vcab_sigab, vcab_sigbb, &
                        rho_a, rho_b, sigma_aa, sigma_ab, sigma_bb, MCSv3_Gopp, MCSv2_Fab)
                  call vcaa_0_deriv(vcaa, vcaa_rhoa, vcaa_sigaa, vcaa_taua, rho_a, sigma_aa, tau_a, MCSv3_Gpar, MCSv2_Faa)
                  vc0 = TWO * (vcaa + vcab)
                  epsc0 = TWO * (vcaa + vcab) / rho
                  vc0_rho = vcaa_rhoa + vcab_rhoa + vcab_rhob
                  vc0_sigma = (vcaa_sigaa + vcab_sigaa + vcab_sigab + vcab_sigbb) * (ONE/TWO)
                  vc0_tau = vcaa_taua
                  !
                  ! Vc(lambda->Infinity) and its derivatives
                  !
                  call u_tpss_vc_inf(epscinf, vcinf_rhoa, vcinf_rhob, vcinf_sigaa, vcinf_sigab, vcinf_sigbb, &
                        vcinf_taua, vcinf_taub, rho_a, rho_b, sigma_aa, sigma_ab, sigma_bb, tau_a, tau_b)
                  vcinf = rho * epscinf
                  vcinf_rho = vcinf_rhoa
                  vcinf_sigma = vcinf_sigaa * (ONE/TWO) + vcinf_sigab * (ONE/FOUR)
                  vcinf_tau = vcinf_taua
                  !
                  ! Use the AC-D model at lambda=1 (Eq. 59 in Ref. 2)
                  !
                  epsc = epscinf * epsc0 / (TWO * epscinf + epsc0)
                  ec = rho * epsc
                  ec_vcinf = vc0**2 / (TWO * vcinf + vc0)**2
                  ec_vc0 = TWO * vcinf**2 / (TWO * vcinf + vc0)**2
                  ec_rho = ec_vcinf * vcinf_rho + ec_vc0 * vc0_rho
                  ec_sigma = ec_vcinf * vcinf_sigma + ec_vc0 * vc0_sigma
                  ec_tau = ec_vcinf * vcinf_tau + ec_vc0 * vc0_tau
            else
                  epsc = ZERO
                  ec_rho = ZERO
                  ec_sigma = ZERO
                  ec_tau = ZERO
            end if
      end subroutine modrzej2016_acd_c_tpss


      subroutine u_modrzej2016_acd_c(epsc, ec_rhoa, ec_rhob, ec_sigaa, ec_sigab, ec_sigbb, ec_taua, ec_taub, &
            rho_a, rho_b, sigma_aa, sigma_ab, sigma_bb, tau_a, tau_b)
            !
            ! Compute the correlation energy of Modrzejewski et al. using the AC-D model
            ! of the adiabatic connection integrand.
            !
            ! This subroutine is for spin-uncompensated densities, including fully polarized (|zeta|=1)
            ! systems. This subroutine was checked with Mathematica.
            !
            ! 1. Modrzejewski et al. unpublished
            ! 2. Teale, A.M., Coriani, S., Helgaker, T. J. Chem. Phys. 132, 164115 (2010);
            !    doi: 10.1063/1.3380834
            ! 3. Modrzejewski et al. J. Chem. Phys. 137, 204121 (2012);
            !    doi: 10.1063/1.4768228
            !
            real(F64), intent(out) :: epsc
            real(F64), intent(out) :: ec_rhoa
            real(F64), intent(out) :: ec_rhob
            real(F64), intent(out) :: ec_sigaa
            real(F64), intent(out) :: ec_sigab
            real(F64), intent(out) :: ec_sigbb
            real(F64), intent(out) :: ec_taua
            real(F64), intent(out) :: ec_taub
            real(F64), intent(in)  :: rho_a
            real(F64), intent(in)  :: rho_b
            real(F64), intent(in)  :: sigma_aa
            real(F64), intent(in)  :: sigma_ab
            real(F64), intent(in)  :: sigma_bb
            real(F64), intent(in)  :: tau_a
            real(F64), intent(in)  :: tau_b

            real(F64) :: vc0, epsc0, vc0_rhoa, vc0_rhob, vc0_sigaa, vc0_sigab, vc0_sigbb, vc0_taua, vc0_taub
            real(F64) :: vcinf, epscinf, vcinf_rhoa, vcinf_rhob, vcinf_sigaa, vcinf_sigab, vcinf_sigbb
            real(F64) :: vcinf_tau, ec_vcinf, ec_vc0, rho
            real(F64) :: vcab, vcab_rhoa, vcab_rhob, vcab_sigaa, vcab_sigab, vcab_sigbb
            real(F64) :: vcaa, vcaa_rhoa, vcaa_sigaa, vcaa_taua
            real(F64) :: vcbb, vcbb_rhob, vcbb_sigbb, vcbb_taub

            epsc = ZERO
            ec_rhoa = ZERO
            ec_rhob = ZERO
            ec_sigaa = ZERO
            ec_sigab = ZERO
            ec_sigbb = ZERO
            ec_taua = ZERO
            ec_taub = ZERO

            if (rho_a > MODRZEJ2016_C_RHO_THRESH .and. rho_b > MODRZEJ2016_C_RHO_THRESH) then
                  rho = rho_a + rho_b
                  !
                  ! d/dlambda Vc(lambda)|lambda->0 and its derivatives
                  !
                  call vcab_0_deriv(vcab, vcab_rhoa, vcab_rhob, vcab_sigaa, vcab_sigab, vcab_sigbb, &
                        rho_a, rho_b, sigma_aa, sigma_ab, sigma_bb, MCSv2_Gopp, MCSv2_Fab)
                  call vcaa_0_deriv(vcaa, vcaa_rhoa, vcaa_sigaa, vcaa_taua, rho_a, sigma_aa, tau_a, MCSv2_Gpar, MCSv2_Faa)
                  call vcaa_0_deriv(vcbb, vcbb_rhob, vcbb_sigbb, vcbb_taub, rho_b, sigma_bb, tau_b, MCSv2_Gpar, MCSv2_Faa)
                  vc0 = vcaa + TWO * vcab + vcbb
                  epsc0 = vc0 / rho
                  vc0_rhoa = vcaa_rhoa + TWO * vcab_rhoa
                  vc0_rhob = vcbb_rhob + TWO * vcab_rhob
                  vc0_sigaa = vcaa_sigaa + TWO * vcab_sigaa
                  vc0_sigab = TWO * vcab_sigab
                  vc0_sigbb = vcbb_sigbb + TWO * vcab_sigbb
                  vc0_taua = vcaa_taua
                  vc0_taub = vcbb_taub
                  !
                  ! Vc(lambda->Infinity) and its derivatives
                  !
                  call u_point_charge_c(epscinf, vcinf_rhoa, vcinf_rhob, rho_a, rho_b)
                  vcinf = rho * epscinf
                  vcinf_sigaa = ZERO
                  vcinf_sigab = ZERO
                  vcinf_sigbb = ZERO
                  vcinf_tau = ZERO
                  !
                  ! AC-D model at lambda=1 (Eq. 59 in Ref. 2)
                  !
                  epsc = epscinf * epsc0 / (TWO * epscinf + epsc0)
                  ec_vcinf = vc0**2 / (TWO * vcinf + vc0)**2
                  ec_vc0 = TWO * vcinf**2 / (TWO * vcinf + vc0)**2
                  ec_rhoa = ec_vcinf * vcinf_rhoa + ec_vc0 * vc0_rhoa
                  ec_rhob = ec_vcinf * vcinf_rhob + ec_vc0 * vc0_rhob
                  ec_sigaa = ec_vcinf * vcinf_sigaa + ec_vc0 * vc0_sigaa
                  ec_sigab = ec_vcinf * vcinf_sigab + ec_vc0 * vc0_sigab
                  ec_sigbb = ec_vcinf * vcinf_sigbb + ec_vc0 * vc0_sigbb
                  ec_taua = ec_vcinf * vcinf_tau + ec_vc0 * vc0_taua
                  ec_taub = ec_vcinf * vcinf_tau + ec_vc0 * vc0_taub
            else if (rho_a > MODRZEJ2016_C_RHO_THRESH) then
                  rho = rho_a
                  !
                  ! d/dlambda Vc(lambda)|lambda->0 and its derivatives
                  !
                  call vcaa_0_deriv(vcaa, vcaa_rhoa, vcaa_sigaa, vcaa_taua, rho_a, sigma_aa, tau_a, MCSv2_Gpar, MCSv2_Faa)
                  vc0 = vcaa
                  epsc0 = vc0 / rho
                  vc0_rhoa = vcaa_rhoa
                  vc0_rhob = ZERO
                  vc0_sigaa = vcaa_sigaa
                  vc0_sigab = ZERO
                  vc0_sigbb = ZERO
                  vc0_taua = vcaa_taua
                  vc0_taub = ZERO
                  !
                  ! Vc(lambda->Infinity) and its derivatives
                  !
                  call u_point_charge_c(epscinf, vcinf_rhoa, vcinf_rhob, rho_a, ZERO)
                  vcinf = rho * epscinf
                  vcinf_rhob = ZERO
                  vcinf_sigaa = ZERO
                  vcinf_sigab = ZERO
                  vcinf_sigbb = ZERO
                  vcinf_tau = ZERO
                  !
                  ! AC-D model at lambda=1 (Eq. 59 in Ref. 2)
                  !
                  epsc = epscinf * epsc0 / (TWO * epscinf + epsc0)
                  ec_vcinf = vc0**2 / (TWO * vcinf + vc0)**2
                  ec_vc0 = TWO * vcinf**2 / (TWO * vcinf + vc0)**2
                  ec_rhoa = ec_vcinf * vcinf_rhoa + ec_vc0 * vc0_rhoa
                  ec_sigaa = ec_vcinf * vcinf_sigaa + ec_vc0 * vc0_sigaa
                  ec_taua = ec_vcinf * vcinf_tau + ec_vc0 * vc0_taua
            else if (rho_b > MODRZEJ2016_C_RHO_THRESH) then
                  rho = rho_b
                  !
                  ! d/dlambda Vc(lambda)|lambda->0 and its derivatives
                  !
                  call vcaa_0_deriv(vcbb, vcbb_rhob, vcbb_sigbb, vcbb_taub, rho_b, sigma_bb, tau_b, MCSv2_Gpar, MCSv2_Faa)
                  vc0 = vcbb
                  epsc0 = vc0 / rho
                  vc0_rhoa = ZERO
                  vc0_rhob = vcbb_rhob
                  vc0_sigaa = ZERO
                  vc0_sigab = ZERO
                  vc0_sigbb = vcbb_sigbb
                  vc0_taua = ZERO
                  vc0_taub = vcbb_taub
                  !
                  ! Vc(lambda->Infinity) and its derivatives
                  !
                  call u_point_charge_c(epscinf, vcinf_rhoa, vcinf_rhob, ZERO, rho_b)
                  vcinf = rho * epscinf
                  vcinf_rhoa = ZERO
                  vcinf_sigaa = ZERO
                  vcinf_sigab = ZERO
                  vcinf_sigbb = ZERO
                  vcinf_tau = ZERO
                  !
                  ! AC-D model at lambda=1 (Eq. 59 in Ref. 2)
                  !
                  epsc = epscinf * epsc0 / (TWO * epscinf + epsc0)
                  ec_vcinf = vc0**2 / (TWO * vcinf + vc0)**2
                  ec_vc0 = TWO * vcinf**2 / (TWO * vcinf + vc0)**2
                  ec_rhob = ec_vcinf * vcinf_rhob + ec_vc0 * vc0_rhob
                  ec_sigbb = ec_vcinf * vcinf_sigbb + ec_vc0 * vc0_sigbb
                  ec_taub = ec_vcinf * vcinf_tau + ec_vc0 * vc0_taub
            end if
      end subroutine u_modrzej2016_acd_c


      subroutine u_modrzej2016_acd_c_tpss(epsc, ec_rhoa, ec_rhob, ec_sigaa, ec_sigab, ec_sigbb, ec_taua, ec_taub, &
            rho_a, rho_b, sigma_aa, sigma_ab, sigma_bb, tau_a, tau_b)
            !
            ! Compute the correlation energy of Modrzejewski et al. using the AC-D model
            ! of the adiabatic connection integrand.
            !
            ! This subroutine is for spin-uncompensated densities, including fully polarized (|zeta|=1)
            ! systems. This subroutine was checked with Mathematica.
            !
            ! 1. Modrzejewski et al. unpublished
            ! 2. Teale, A.M., Coriani, S., Helgaker, T. J. Chem. Phys. 132, 164115 (2010);
            !    doi: 10.1063/1.3380834
            ! 3. Modrzejewski et al. J. Chem. Phys. 137, 204121 (2012);
            !    doi: 10.1063/1.4768228
            !
            real(F64), intent(out) :: epsc
            real(F64), intent(out) :: ec_rhoa
            real(F64), intent(out) :: ec_rhob
            real(F64), intent(out) :: ec_sigaa
            real(F64), intent(out) :: ec_sigab
            real(F64), intent(out) :: ec_sigbb
            real(F64), intent(out) :: ec_taua
            real(F64), intent(out) :: ec_taub
            real(F64), intent(in)  :: rho_a
            real(F64), intent(in)  :: rho_b
            real(F64), intent(in)  :: sigma_aa
            real(F64), intent(in)  :: sigma_ab
            real(F64), intent(in)  :: sigma_bb
            real(F64), intent(in)  :: tau_a
            real(F64), intent(in)  :: tau_b

            real(F64) :: vc0, epsc0, vc0_rhoa, vc0_rhob, vc0_sigaa, vc0_sigab, vc0_sigbb, vc0_taua, vc0_taub
            real(F64) :: vcinf, epscinf, vcinf_rhoa, vcinf_rhob, vcinf_sigaa, vcinf_sigab, vcinf_sigbb
            real(F64) :: vcinf_taua, vcinf_taub, ec_vcinf, ec_vc0, rho, Da, Db
            real(F64) :: vcab, vcab_rhoa, vcab_rhob, vcab_sigaa, vcab_sigab, vcab_sigbb
            real(F64) :: vcaa, vcaa_rhoa, vcaa_sigaa, vcaa_taua
            real(F64) :: vcbb, vcbb_rhob, vcbb_sigbb, vcbb_taub
            real(F64), parameter :: self_int_thresh = 1.0E-15_F64

            epsc = ZERO
            ec_rhoa = ZERO
            ec_rhob = ZERO
            ec_sigaa = ZERO
            ec_sigab = ZERO
            ec_sigbb = ZERO
            ec_taua = ZERO
            ec_taub = ZERO

            if (rho_a > MODRZEJ2016_C_RHO_THRESH .and. rho_b > MODRZEJ2016_C_RHO_THRESH) then
                  rho = rho_a + rho_b
                  !
                  ! d/dlambda Vc(lambda)|lambda->0 and its derivatives
                  !
                  call vcab_0_deriv(vcab, vcab_rhoa, vcab_rhob, vcab_sigaa, vcab_sigab, vcab_sigbb, &
                        rho_a, rho_b, sigma_aa, sigma_ab, sigma_bb, MCSv3_Gopp, MCSv2_Fab)
                  call vcaa_0_deriv(vcaa, vcaa_rhoa, vcaa_sigaa, vcaa_taua, rho_a, sigma_aa, tau_a, MCSv3_Gpar, MCSv2_Faa)
                  call vcaa_0_deriv(vcbb, vcbb_rhob, vcbb_sigbb, vcbb_taub, rho_b, sigma_bb, tau_b, MCSv3_Gpar, MCSv2_Faa)
                  vc0 = vcaa + TWO * vcab + vcbb
                  epsc0 = vc0 / rho
                  vc0_rhoa = vcaa_rhoa + TWO * vcab_rhoa
                  vc0_rhob = vcbb_rhob + TWO * vcab_rhob
                  vc0_sigaa = vcaa_sigaa + TWO * vcab_sigaa
                  vc0_sigab = TWO * vcab_sigab
                  vc0_sigbb = vcbb_sigbb + TWO * vcab_sigbb
                  vc0_taua = vcaa_taua
                  vc0_taub = vcbb_taub
                  !
                  ! Vc(lambda->Infinity) and its derivatives
                  !
                  call u_tpss_vc_inf(epscinf, vcinf_rhoa, vcinf_rhob, vcinf_sigaa, vcinf_sigab, vcinf_sigbb, &
                        vcinf_taua, vcinf_taub, rho_a, rho_b, sigma_aa, sigma_ab, sigma_bb, tau_a, tau_b)
                  vcinf = rho * epscinf
                  !
                  ! AC-D model at lambda=1 (Eq. 59 in Ref. 2)
                  !
                  epsc = epscinf * epsc0 / (TWO * epscinf + epsc0)
                  ec_vcinf = vc0**2 / (TWO * vcinf + vc0)**2
                  ec_vc0 = TWO * vcinf**2 / (TWO * vcinf + vc0)**2
                  ec_rhoa = ec_vcinf * vcinf_rhoa + ec_vc0 * vc0_rhoa
                  ec_rhob = ec_vcinf * vcinf_rhob + ec_vc0 * vc0_rhob
                  ec_sigaa = ec_vcinf * vcinf_sigaa + ec_vc0 * vc0_sigaa
                  ec_sigab = ec_vcinf * vcinf_sigab + ec_vc0 * vc0_sigab
                  ec_sigbb = ec_vcinf * vcinf_sigbb + ec_vc0 * vc0_sigbb
                  ec_taua = ec_vcinf * vcinf_taua + ec_vc0 * vc0_taua
                  ec_taub = ec_vcinf * vcinf_taub + ec_vc0 * vc0_taub
            else if (rho_a > MODRZEJ2016_C_RHO_THRESH) then
                  !
                  ! Check if this is a fully spin-polarized single-orbital density
                  ! to avoid division by zero when evaluating the AC-D formula.
                  ! (In that case both Vc(lambda->Inf) and dVc/dlambda(lambda->0) vanish.)
                  !
                  Da = abs(tau_a - sigma_aa / (FOUR * rho_a))
                  if (Da > self_int_thresh) then
                        rho = rho_a
                        !
                        ! d/dlambda Vc(lambda)|lambda->0 and its derivatives
                        !
                        call vcaa_0_deriv(vcaa, vcaa_rhoa, vcaa_sigaa, vcaa_taua, rho_a, sigma_aa, tau_a, MCSv3_Gpar, MCSv2_Faa)
                        vc0 = vcaa
                        epsc0 = vc0 / rho
                        vc0_rhoa = vcaa_rhoa
                        vc0_rhob = ZERO
                        vc0_sigaa = vcaa_sigaa
                        vc0_sigab = ZERO
                        vc0_sigbb = ZERO
                        vc0_taua = vcaa_taua
                        vc0_taub = ZERO
                        !
                        ! Vc(lambda->Infinity) and its derivatives
                        !
                        call u_tpss_vc_inf(epscinf, vcinf_rhoa, vcinf_rhob, vcinf_sigaa, vcinf_sigab, vcinf_sigbb, &
                              vcinf_taua, vcinf_taub, rho_a, rho_b, sigma_aa, sigma_ab, sigma_bb, tau_a, tau_b)
                        vcinf = rho * epscinf
                        !
                        ! AC-D model at lambda=1 (Eq. 59 in Ref. 2)
                        !
                        epsc = epscinf * epsc0 / (TWO * epscinf + epsc0)
                        ec_vcinf = vc0**2 / (TWO * vcinf + vc0)**2
                        ec_vc0 = TWO * vcinf**2 / (TWO * vcinf + vc0)**2
                        ec_rhoa = ec_vcinf * vcinf_rhoa + ec_vc0 * vc0_rhoa
                        ec_sigaa = ec_vcinf * vcinf_sigaa + ec_vc0 * vc0_sigaa
                        ec_taua = ec_vcinf * vcinf_taua + ec_vc0 * vc0_taua
                  end if
            else if (rho_b > MODRZEJ2016_C_RHO_THRESH) then
                  Db = abs(tau_b - sigma_bb / (FOUR * rho_b))
                  if (Db > self_int_thresh) then
                        rho = rho_b
                        !
                        ! d/dlambda Vc(lambda)|lambda->0 and its derivatives
                        !
                        call vcaa_0_deriv(vcbb, vcbb_rhob, vcbb_sigbb, vcbb_taub, rho_b, sigma_bb, tau_b, MCSv3_Gpar, MCSv2_Faa)
                        vc0 = vcbb
                        epsc0 = vc0 / rho
                        vc0_rhoa = ZERO
                        vc0_rhob = vcbb_rhob
                        vc0_sigaa = ZERO
                        vc0_sigab = ZERO
                        vc0_sigbb = vcbb_sigbb
                        vc0_taua = ZERO
                        vc0_taub = vcbb_taub
                        !
                        ! Vc(lambda->Infinity) and its derivatives
                        !
                        call u_tpss_vc_inf(epscinf, vcinf_rhoa, vcinf_rhob, vcinf_sigaa, vcinf_sigab, vcinf_sigbb, &
                              vcinf_taua, vcinf_taub, rho_a, rho_b, sigma_aa, sigma_ab, sigma_bb, tau_a, tau_b)
                        vcinf = rho * epscinf
                        !
                        ! AC-D model at lambda=1 (Eq. 59 in Ref. 2)
                        !
                        epsc = epscinf * epsc0 / (TWO * epscinf + epsc0)
                        ec_vcinf = vc0**2 / (TWO * vcinf + vc0)**2
                        ec_vc0 = TWO * vcinf**2 / (TWO * vcinf + vc0)**2
                        ec_rhob = ec_vcinf * vcinf_rhob + ec_vc0 * vc0_rhob
                        ec_sigbb = ec_vcinf * vcinf_sigbb + ec_vc0 * vc0_sigbb
                        ec_taub = ec_vcinf * vcinf_taub + ec_vc0 * vc0_taub
                  end if
            end if
      end subroutine u_modrzej2016_acd_c_tpss


      subroutine vcab_0_deriv(vcab, vcab_rhoa, vcab_rhob, vcab_sigaa, vcab_sigab, vcab_sigbb, &
            rho_a, rho_b, sigma_aa, sigma_ab, sigma_bb, gopp, Fab)
            !
            ! Compute the derivative dVcab/dlambda|lambda->0
            ! 1. Modrzejewski et al. J. Chem. Phys. 137, 204121 (2012);
            !    doi: 10.1063/1.4768228
            !
            real(F64), intent(out) :: vcab
            real(F64), intent(out) :: vcab_rhoa
            real(F64), intent(out) :: vcab_rhob
            real(F64), intent(out) :: vcab_sigaa
            real(F64), intent(out) :: vcab_sigab
            real(F64), intent(out) :: vcab_sigbb
            real(F64), intent(in)  :: rho_a
            real(F64), intent(in)  :: rho_b
            real(F64), intent(in)  :: sigma_aa
            real(F64), intent(in)  :: sigma_ab
            real(F64), intent(in)  :: sigma_bb
            real(F64), intent(in)  :: gopp
            real(F64), intent(in)  :: Fab

            real(F64) :: sigma, rsab, rsab_rhoa, rsab_rhob, vcab_chi, rs, rs_rho
            real(F64) :: rhoa13, rhoa43, rhob13, rhob43, rho
            real(F64) :: babEq25, babEq25_rhob, babEq25_rsab
            real(F64) :: aab, aab_rhob, aab_rsab, dab, dab_chi, dab_rsab
            real(F64) :: bab, bab_rhob, bab_rsab, bab_chi, vcab_rsab
            real(F64) :: chi, chi_sigma, chi_sigaa, chi_sigab, chi_sigbb, chi_rho, chi_rhoa, chi_rhob
            !
            ! rsab defined in Eq. 28 of Ref. 1
            !
            rhoa13 = rho_a**(ONE/THREE)
            rhoa43 = rhoa13**4
            rhob13 = rho_b**(ONE/THREE)
            rhob43 = rhob13**4
            rsab = (THREE/PI)**(ONE/THREE) / (rhoa13 + rhob13)
            rsab_rhoa = -ONE/(THREE**(TWO/THREE)*PI**(ONE/THREE)) / (rhoa13**2 * (rhoa13 + rhob13)**2)
            rsab_rhob = -ONE/(THREE**(TWO/THREE)*PI**(ONE/THREE)) / (rhob13**2 * (rhoa13 + rhob13)**2)
            !
            ! rs defined in Eq. 29 of Ref. 1
            !
            rho = rho_a + rho_b
            rs = (THREE / (FOUR*PI * rho))**(ONE/THREE)
            rs_rho = -(ONE/THREE) * rs / rho
            !
            ! Gradient-dependent part of the damping function
            !
            sigma = sigma_aa  + TWO * sigma_ab + sigma_bb
            chi_sigma = ONE / (rs * rho**(EIGHT/THREE))
            chi = sigma * chi_sigma
            chi_sigaa = chi_sigma
            chi_sigab = TWO * chi_sigma
            chi_sigbb = chi_sigma
            chi_rho = -chi / rs * rs_rho - (EIGHT/THREE) * chi / rho
            chi_rhoa = chi_rho
            chi_rhob = chi_rho
            !
            ! dBab/dlambda|lambda->0, Bab defined in Eq. 25 of Ref. 1
            !
            babEq25 = -0.7317_F64 * rho_b * rsab
            babEq25_rhob = -0.7317_F64 * rsab
            babEq25_rsab = -0.7317_F64 * rho_b
            !
            ! daab/dlambda|lambda->0, aab defined in Eq. 32 of Ref. 1
            !
            aab = babEq25
            aab_rhob = babEq25_rhob
            aab_rsab = babEq25_rsab
            !
            ! dab defined in Eq. 53 of Eq. 1
            !
            dab = Fab / rsab + gopp * chi
            dab_chi = gopp
            dab_rsab = -Fab / rsab**2
            !
            ! dbab/dlambda|lambda->0, bab defined in Eq. 33 of Ref. 1
            !
            bab = rho_b + dab * aab
            bab_rhob = ONE + dab * aab_rhob
            bab_rsab = dab_rsab * aab + dab * aab_rsab
            bab_chi = dab_chi * aab
            !
            ! dVcab/dlambda|lambda->0, Vcab defined in Eq. 40 of Ref. 1
            !
            vcab = PI * rho_a * (bab + aab * dab) / dab**3
            vcab_rsab = PI * rho_a * (bab_rsab + aab_rsab * dab + aab * dab_rsab) / dab**3 &
                  - THREE * vcab / dab * dab_rsab
            vcab_chi = PI * rho_a * (bab_chi + aab * dab_chi) / dab**3 - THREE * vcab / dab * dab_chi
            vcab_rhoa = PI * (bab + aab * dab) / dab**3 + vcab_rsab * rsab_rhoa + vcab_chi * chi_rhoa
            vcab_rhob = PI * rho_a * (bab_rhob + aab_rhob * dab) / dab**3 + vcab_rsab * rsab_rhob + vcab_chi * chi_rhob
            vcab_sigab = vcab_chi * chi_sigab
            vcab_sigaa = vcab_chi * chi_sigaa
            vcab_sigbb = vcab_chi * chi_sigbb
      end subroutine vcab_0_deriv


      pure subroutine vcaa_0_deriv(vcaa, vcaa_rhoa, vcaa_sigaa, vcaa_taua, rho_a, sigma_aa, tau_a, gpar, Faa)
            !
            ! Compute the derivative dVcaa/dlambda|lambda->0
            !
            ! 1. Modrzejewski et al. J. Chem. Phys. 137, 204121 (2012);
            !    doi: 10.1063/1.4768228
            !
            real(F64), intent(out) :: vcaa
            real(F64), intent(out) :: vcaa_rhoa
            real(F64), intent(out) :: vcaa_sigaa
            real(F64), intent(out) :: vcaa_taua
            real(F64), intent(in)  :: rho_a
            real(F64), intent(in)  :: sigma_aa
            real(F64), intent(in)  :: tau_a
            real(F64), intent(in)  :: gpar
            real(F64), intent(in)  :: Faa

            real(F64) :: rsaa, rsaa_rhoa, baaEq31, baaEq31_rsaa, baaEq31_Da
            real(F64) :: aaa, aaa_rsaa, aaa_Da, daa, daa_chi, daa_rsaa
            real(F64) :: baa, baa_rsaa, baa_Da, baa_chi, vcaa_rsaa
            real(F64) :: chi, chi_sigaa, chi_rhoa
            real(F64) :: vcaa_Da, vcaa_chi, rhoa13, rhoa83
            real(F64) :: Da, Da_taua, Da_sigaa, Da_rhoa
            !
            ! rsab defined in Eq. 27 of Ref. 1
            !
            rhoa13 = rho_a**(ONE/THREE)
            rhoa83 = rhoa13**8
            rsaa = (THREE/PI)**(ONE/THREE)/TWO / rhoa13
            rsaa_rhoa = -(ONE/THREE) * rsaa / rho_a
            !
            ! Gradient-dependent part of the damping function
            ! (spin dependence changed with respect to Ref. 1, where
            ! the total gradient and density were used)
            !
            chi = sigma_aa / (rsaa * rhoa83)
            chi_sigaa = ONE / (rsaa * rhoa83)
            chi_rhoa = -chi / rsaa * rsaa_rhoa - (EIGHT/THREE) * chi / rho_a
            !
            ! Self-interaction factor
            !
            Da = tau_a - sigma_aa / (FOUR * rho_a)
            Da_taua = ONE
            Da_sigaa = -ONE / (FOUR * rho_a)
            Da_rhoa = sigma_aa / (FOUR * rho_a**2)
            !
            ! dBaa/dlambda|lambda->0, Baa defined in Eq. 31 of Ref. 1
            !
            baaEq31 = -0.57284_F64 * Da/THREE * rsaa
            baaEq31_rsaa = -0.57284_F64 * Da/THREE
            baaEq31_Da = -0.57284_F64/THREE * rsaa
            !
            ! daaa/dlambda|lambda->0, aaa defined in Eq. 36 of Ref. 1
            !
            aaa = baaEq31
            aaa_rsaa = baaEq31_rsaa
            aaa_Da = baaEq31_Da
            !
            ! daa defined in Eq. 54 of Eq. 1
            !
            daa = Faa / rsaa + gpar * chi
            daa_chi = gpar
            daa_rsaa = -Faa / rsaa**2
            !
            ! dbaa/dlambda|lambda->0, baa defined in Eq. 37 of Ref. 1
            !
            baa = (ONE/TWO) * Da/THREE + aaa * daa
            baa_rsaa = aaa_rsaa * daa + aaa * daa_rsaa
            baa_Da = (ONE/TWO)/THREE + aaa_Da * daa
            baa_chi = aaa * daa_chi
            !
            ! dVcaa/dlambda|lambda->0, Vcaa defined in Eq. 41 of Ref. 1
            !
            vcaa = PI * rho_a * (EIGHT * baa + FOUR * aaa * daa) / daa**5
            vcaa_rsaa = PI * rho_a * (EIGHT * baa_rsaa + FOUR * (aaa_rsaa * daa + aaa * daa_rsaa)) / daa**5 &
                  - FIVE * vcaa / daa * daa_rsaa
            vcaa_Da = PI * rho_a * (EIGHT * baa_Da + FOUR * aaa_Da * daa) / daa**5
            vcaa_chi = PI * rho_a * (EIGHT * baa_chi + FOUR * aaa * daa_chi) / daa**5 &
                  - FIVE * vcaa / daa * daa_chi
            vcaa_rhoa = PI * (EIGHT * baa + FOUR * aaa * daa) / daa**5 + vcaa_rsaa * rsaa_rhoa &
                  + vcaa_chi * chi_rhoa + vcaa_Da * Da_rhoa
            vcaa_sigaa = vcaa_chi * chi_sigaa + vcaa_Da * Da_sigaa
            vcaa_taua = vcaa_Da * Da_taua
      end subroutine vcaa_0_deriv
      

      pure subroutine point_charge_c(epsc, ec_rho, rho)
            !
            ! Correlation part of the point charge plus continuum model of Vxc(lambda->Inf),
            ! without any gradient corrections. To get the correlation contribution, the LDA exchange
            ! is subtracted from the RHS of Eq. 16 in Ref. 1.
            !
            ! Note that the PC formula for Vxc(lambda->Inf) is independent of spin polarization,
            ! but Vc(lambda->Inf) depends on spin. This version of the subroutine is for spin-compensated
            ! systems.
            !
            ! 1. Seidl, M., Perdew, J.P., Kurth, S., Phys. Rev. A, 62, 012502 (2000);
            !    doi: 10.1103/PhysRevA.62.012502
            !
            real(F64), intent(out) :: epsc
            real(F64), intent(out) :: ec_rho
            real(F64), intent(in)  :: rho

            real(F64), parameter :: Kxc = -(NINE/TEN)*(FOUR*PI/THREE)**(ONE/THREE)
            real(F64), parameter :: Kx = (-THREE/FOUR)*(THREE/PI)**(ONE/THREE)
            real(F64), parameter :: Kc = Kxc - Kx

            epsc = Kc * rho**(ONE/THREE)
            ec_rho = (FOUR/THREE) * epsc
      end subroutine point_charge_c


      pure subroutine u_point_charge_c(epsc, ec_rhoa, ec_rhob, rho_a, rho_b)
            !
            ! Correlation part of the point charge plus continuum model of Vxc(lambda->Inf),
            ! without any gradient corrections. To get the correlation contribution, the LDA exchange
            ! is subtracted from the RHS of Eq. 16 in Ref. 1.
            !
            ! Note that the PC formula for Vxc(lambda->Inf) is independent of spin polarization,
            ! but Vc(lambda->Inf) depends on spin. This version of the subroutine is for spin-polarized
            ! systems. 
            !
            ! 1. Seidl, M., Perdew, J.P., Kurth, S., Phys. Rev. A, 62, 012502 (2000);
            !    doi: 10.1103/PhysRevA.62.012502
            !
            real(F64), intent(out) :: epsc
            real(F64), intent(out) :: ec_rhoa
            real(F64), intent(out) :: ec_rhob
            real(F64), intent(in)  :: rho_a
            real(F64), intent(in)  :: rho_b

            real(F64) :: exa_rhoa, exb_rhob, ex_a, ex_b, rho_tot, epsxc, exc_rho
            real(F64), parameter :: Kxc = -(NINE/TEN)*(FOUR*PI/THREE)**(ONE/THREE)
            real(F64), parameter :: Kx = (-THREE/FOUR)*(THREE/PI)**(ONE/THREE)

            exa_rhoa = (FOUR/THREE) * Kx * (TWO * rho_a)**(ONE/THREE)
            ex_a = rho_a * (THREE/FOUR) * exa_rhoa
            exb_rhob = (FOUR/THREE) * Kx * (TWO * rho_b)**(ONE/THREE)
            ex_b = rho_b * (THREE/FOUR) * exb_rhob
            rho_tot = rho_a + rho_b
            epsxc = Kxc * rho_tot**(ONE/THREE)
            epsc = epsxc - (ex_a + ex_b) / rho_tot
            exc_rho = (FOUR/THREE) * epsxc
            ec_rhoa = exc_rho - exa_rhoa
            ec_rhob = exc_rho - exb_rhob
      end subroutine u_point_charge_c
end module modrzej2016_c_energy
