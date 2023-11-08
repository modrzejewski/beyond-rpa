module tpss_c_energy
      use arithmetic
      use math_constants
      use pbe_c_energy

      implicit none
      !
      ! Parameters of C(zeta,0) defined in Eq. 4 of the revTPSS paper Phys. Rev. Lett. 103, 026403 (2009);
      ! doi: 10.1103/PhysRevLett.103.026403
      !
      real(F64), dimension(4), parameter :: REVTPSS_C_COEFFS = [0.59_F64, 0.9269_F64, 0.6225_F64, 2.1540_F64]
      !
      ! Parameters of C(zeta,0) defined in Eq. 33 of the TPSS paper J. Chem. Pnys. 120, 6898 (2004);
      ! doi: 10.1063/1.1665298
      !
      real(F64), dimension(4), parameter :: TPSS_C_COEFFS = [0.53_F64, 0.87_F64, 0.50_F64, 2.26_F64]
      !
      ! Parameter d defined in Eq. 32 of J. Chem. Phys. 120, 6898 (2004); doi: 10.1063/1.1665298
      ! The same value is used in revTPSS
      !
      real(F64), parameter :: D_TPSS = 2.8_F64

      real(F64), parameter :: TPSS_C_TAU_THRESH = 1.0E-12_F64
      
contains

      subroutine revtpss_c(epsc, ec_rho, ec_sigma, ec_tau, rho, sigma, tau)
            !
            ! Compute the revTPSS correlation energy for a spin-unpolarized densities.
            !
            ! 1. Perdew, J.P., Ruzsinszky, A., Csonka, G., Constantin, L.A., Sun, J.,
            !    Phys. Rev. Lett. 103, 026403 (2009); doi: 10.1103/PhysRevLett.103.026403
            !
            real(F64), intent(out) :: epsc
            real(F64), intent(out) :: ec_rho
            real(F64), intent(out) :: ec_sigma
            real(F64), intent(out) :: ec_tau
            real(F64), intent(in)  :: rho
            real(F64), intent(in)  :: sigma
            real(F64), intent(in)  :: tau

            real(F64) :: epscgga_tot, ecgga_tot_rho, ecgga_tot_sigma
            real(F64) :: epscgga_a, ecgga_a_rhoa, ecgga_a_rhob, ecgga_a_sigaa, ecgga_a_sigab, ecgga_a_sigbb

            call mvs_c(epscgga_tot, ecgga_tot_rho, ecgga_tot_sigma, rho, sigma)
            call u_mvs_c(epscgga_a, ecgga_a_rhoa, ecgga_a_rhob, ecgga_a_sigaa, ecgga_a_sigab, &
                  ecgga_a_sigbb, rho/TWO, ZERO, sigma/FOUR, ZERO, ZERO)            
            call tpss_c_template(epsc, ec_rho, ec_sigma, ec_tau, rho, sigma, tau, revTPSS_C_COEFFS, &
                  epscgga_tot, ecgga_tot_rho, ecgga_tot_sigma, epscgga_a, ecgga_a_rhoa, ecgga_a_sigaa)
      end subroutine revtpss_c


      subroutine tpss_c(epsc, ec_rho, ec_sigma, ec_tau, rho, sigma, tau)
            !
            ! Compute the TPSS correlation energy for a spin-unpolarized densities.
            !
            ! Correctness of the code tested against NWChem 6.5 and the Fortran code 
            ! of E. Fabiano and L.A. Constantin.
            !
            ! 1. Perdew, J.P., Tao, J., Staroverov, V.N., Scuseria, G.E.,
            !    J. Chem. Phys. 120, 6898 (2004); doi: 10.1063/1.1665298
            !
            real(F64), intent(out) :: epsc
            real(F64), intent(out) :: ec_rho
            real(F64), intent(out) :: ec_sigma
            real(F64), intent(out) :: ec_tau
            real(F64), intent(in)  :: rho
            real(F64), intent(in)  :: sigma
            real(F64), intent(in)  :: tau

            real(F64) :: epscgga_tot, ecgga_tot_rho, ecgga_tot_sigma
            real(F64) :: epscgga_a, ecgga_a_rhoa, ecgga_a_rhob, ecgga_a_sigaa, ecgga_a_sigab, ecgga_a_sigbb

            call pbe_c(epscgga_tot, ecgga_tot_rho, ecgga_tot_sigma, rho, sigma)
            call u_pbe_c(epscgga_a, ecgga_a_rhoa, ecgga_a_rhob, ecgga_a_sigaa, ecgga_a_sigab, &
                  ecgga_a_sigbb, rho/TWO, ZERO, sigma/FOUR, ZERO, ZERO)            
            call tpss_c_template(epsc, ec_rho, ec_sigma, ec_tau, rho, sigma, tau, TPSS_C_COEFFS, &
                  epscgga_tot, ecgga_tot_rho, ecgga_tot_sigma, epscgga_a, ecgga_a_rhoa, ecgga_a_sigaa)
      end subroutine tpss_c


      subroutine u_tpss_c(epsc, ec_rhoa, ec_rhob, ec_sigaa, ec_sigab, ec_sigbb, ec_taua, ec_taub, &
            rho_a, rho_b, sigma_aa, sigma_ab, sigma_bb, tau_a, tau_b)
            !
            ! Compute the TPSS correlation energy for a spin-polarized densities.
            ! Fully spin-polarized densities are admissible. Checking for small densities
            ! is done inside this subroutine.
            !
            ! Correctness of the code is tested against NWChem 6.5 and the Fortran code 
            ! of E. Fabiano and L.A. Constantin. 
            ! (There is a bug in NWChem's TPSS correlation for highly spin-polarized systems,
            ! which I detected by comparing NWChem's code to this subroutine.)
            !
            ! 1. Perdew, J.P., Tao, J., Staroverov, V.N., Scuseria, G.E.,
            !    J. Chem. Phys. 120, 6898 (2004); doi: 10.1063/1.1665298
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
            
            real(F64) :: epscgga_tot, ecgga_tot_rhoa, ecgga_tot_rhob
            real(F64) :: ecgga_tot_sigab, ecgga_tot_sigbb, ecgga_tot_sigaa
            real(F64) :: epscgga_a, ecgga_a_rhoa, ecgga_a_rhob, ecgga_a_sigaa, ecgga_a_sigab, ecgga_a_sigbb
            real(F64) :: epscgga_b, ecgga_b_rhoa, ecgga_b_rhob, ecgga_b_sigaa, ecgga_b_sigab, ecgga_b_sigbb

            call u_pbe_c(epscgga_tot, ecgga_tot_rhoa, ecgga_tot_rhob, ecgga_tot_sigaa, ecgga_tot_sigab, &
                  ecgga_tot_sigbb, rho_a, rho_b, sigma_aa, sigma_ab, sigma_bb)
            call u_pbe_c(epscgga_a, ecgga_a_rhoa, ecgga_a_rhob, ecgga_a_sigaa, ecgga_a_sigab, &
                  ecgga_a_sigbb, rho_a, ZERO, sigma_aa, ZERO, ZERO)
            call u_pbe_c(epscgga_b, ecgga_b_rhoa, ecgga_b_rhob, ecgga_b_sigaa, ecgga_b_sigab, &
                  ecgga_b_sigbb, ZERO, rho_b, ZERO, ZERO, sigma_bb)
            call u_tpss_c_template(epsc, ec_rhoa, ec_rhob, ec_sigaa, ec_sigab, ec_sigbb, &
                  ec_taua, ec_taub, rho_a, rho_b, sigma_aa, sigma_ab, sigma_bb, tau_a, tau_b, TPSS_C_COEFFS, &
                  epscgga_tot, ecgga_tot_rhoa, ecgga_tot_rhob, ecgga_tot_sigaa, ecgga_tot_sigab, ecgga_tot_sigbb, &
                  epscgga_a, ecgga_a_rhoa, ecgga_a_sigaa, epscgga_b, ecgga_b_rhob, ecgga_b_sigbb)
      end subroutine u_tpss_c


      subroutine u_revtpss_c(epsc, ec_rhoa, ec_rhob, ec_sigaa, ec_sigab, ec_sigbb, ec_taua, ec_taub, &
            rho_a, rho_b, sigma_aa, sigma_ab, sigma_bb, tau_a, tau_b)
            !
            ! Compute the revTPSS correlation energy for a spin-polarized densities.
            ! Fully spin-polarized densities are admissible. Checking for small densities
            ! is done inside this subroutine.
            !
            ! 1. Perdew, J.P., Ruzsinszky, A., Csonka, G., Constantin, L.A., Sun, J.,
            !    Phys. Rev. Lett. 103, 026403 (2009); doi: 10.1103/PhysRevLett.103.026403
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
            
            real(F64) :: epscgga_tot, ecgga_tot_rhoa, ecgga_tot_rhob
            real(F64) :: ecgga_tot_sigab, ecgga_tot_sigbb, ecgga_tot_sigaa
            real(F64) :: epscgga_a, ecgga_a_rhoa, ecgga_a_rhob, ecgga_a_sigaa, ecgga_a_sigab, ecgga_a_sigbb
            real(F64) :: epscgga_b, ecgga_b_rhoa, ecgga_b_rhob, ecgga_b_sigaa, ecgga_b_sigab, ecgga_b_sigbb

            call u_mvs_c(epscgga_tot, ecgga_tot_rhoa, ecgga_tot_rhob, ecgga_tot_sigaa, ecgga_tot_sigab, &
                  ecgga_tot_sigbb, rho_a, rho_b, sigma_aa, sigma_ab, sigma_bb)
            call u_mvs_c(epscgga_a, ecgga_a_rhoa, ecgga_a_rhob, ecgga_a_sigaa, ecgga_a_sigab, &
                  ecgga_a_sigbb, rho_a, ZERO, sigma_aa, ZERO, ZERO)
            call u_mvs_c(epscgga_b, ecgga_b_rhoa, ecgga_b_rhob, ecgga_b_sigaa, ecgga_b_sigab, &
                  ecgga_b_sigbb, ZERO, rho_b, ZERO, ZERO, sigma_bb)
            call u_tpss_c_template(epsc, ec_rhoa, ec_rhob, ec_sigaa, ec_sigab, ec_sigbb, &
                  ec_taua, ec_taub, rho_a, rho_b, sigma_aa, sigma_ab, sigma_bb, tau_a, tau_b, revTPSS_C_COEFFS, &
                  epscgga_tot, ecgga_tot_rhoa, ecgga_tot_rhob, ecgga_tot_sigaa, ecgga_tot_sigab, ecgga_tot_sigbb, &
                  epscgga_a, ecgga_a_rhoa, ecgga_a_sigaa, epscgga_b, ecgga_b_rhob, ecgga_b_sigbb)
      end subroutine u_revtpss_c


      subroutine tpss_z_factor(z, z_rho, z_sig, z_tau, rho, sigma, tau)
            !
            ! Compute z=tau_w/tau and its derivatives.
            ! The input tau includes the 1/2 factor.
            !
            real(F64), intent(out) :: z
            real(F64), intent(out) :: z_rho
            real(F64), intent(out) :: z_sig
            real(F64), intent(out) :: z_tau
            real(F64), intent(in)  :: rho
            real(F64), intent(in)  :: sigma
            real(F64), intent(in)  :: tau

            real(F64) :: tauw, tauw_sig, tauw_rho, w
            real(F64), parameter :: single_orbital_thresh = 1.0E-10_F64
            !
            ! Weizsacker kinetic energy (alpha+beta spin, including the 1/2 factor)
            !
            tauw = sigma / (EIGHT * rho)
            !
            ! Detect if TauW == Tau to handle the special case of the hydrogen molecule.
            ! In H2, TauW/Tau=1 everywhere, but at the bond midpoint the derivatives
            ! d/dSigma and d/dTau go to infinity as 1/Tau. To avoid SCF failure,
            ! we use z=1 as a constant, without computing any derivatives. The single-orbital
            ! condition is triggered in systems such as H2, He, Ne...H, and He...He for large R.
            ! We have not found any system with core electrons in all of its constituents
            ! in which this condition becomes active in regions of space with nonnegligible density.
            !
            w = abs((tauw - tau) / tau)
            if (w < single_orbital_thresh) then
                  z = ONE
                  z_rho = ZERO
                  z_sig = ZERO
                  z_tau = ZERO
            else
                  z = tauw / tau
                  tauw_sig = ONE / (EIGHT * rho)
                  tauw_rho = -sigma / (EIGHT * rho**2)
                  z_rho = tauw_rho / tau
                  z_sig = tauw_sig / tau
                  z_tau = -z / tau
            end if
      end subroutine tpss_z_factor
            
      
      subroutine tpss_z3_factor(r, r_rho, r_sig, r_tau, rho, sigma, tau)
            !
            ! Compute (tau_w/(tau/2))**3 and its derivatives.
            ! The input tau does not include the 1/2 factor.
            !
            real(F64), intent(out) :: r
            real(F64), intent(out) :: r_rho
            real(F64), intent(out) :: r_sig
            real(F64), intent(out) :: r_tau
            real(F64), intent(in)  :: rho
            real(F64), intent(in)  :: sigma
            real(F64), intent(in)  :: tau

            real(F64) :: z, z_rho, z_sig, z_tau
            
            call tpss_z_factor(z, z_rho, z_sig, z_tau, rho, sigma, tau/TWO)
            r = z**3
            r_rho = THREE * z**2 * z_rho
            r_sig = THREE * z**2 * z_sig
            !
            ! Take account of the (1/2) factor in the definition of tau
            !
            r_tau = THREE * z**2 * z_tau * (ONE/TWO)
      end subroutine tpss_z3_factor


      subroutine u_tpss_c_template(epsc, ec_rhoa, ec_rhob, ec_sigaa, ec_sigab, ec_sigbb, &
            ec_taua, ec_taub, rho_a, rho_b, sigma_aa, sigma_ab, sigma_bb, tau_a, tau_b, c_coeffs, &
            epscgga_tot, ecgga_tot_rhoa, ecgga_tot_rhob, ecgga_tot_sigaa, ecgga_tot_sigab, ecgga_tot_sigbb, &
            epscgga_a, ecgga_a_rhoa, ecgga_a_sigaa, epscgga_b, ecgga_b_rhob, ecgga_b_sigbb)
            !
            ! Compute the TPSS/revTPSS correlation energy for a spin-polarized densities.
            !
            ! 1. Perdew, J.P., Tao, J., Staroverov, V.N., Scuseria, G.E.,
            !    J. Chem. Phys. 120, 6898 (2004); doi: 10.1063/1.1665298
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
            real(F64), dimension(4), intent(in) :: c_coeffs
            real(F64), intent(in) :: epscgga_tot, ecgga_tot_rhoa, ecgga_tot_rhob, ecgga_tot_sigaa
            real(F64), intent(in) :: ecgga_tot_sigab, ecgga_tot_sigbb
            real(F64), intent(in) :: epscgga_a, ecgga_a_rhoa, ecgga_a_sigaa
            real(F64), intent(in) :: epscgga_b, ecgga_b_rhob, ecgga_b_sigbb

            real(F64) :: wepsc, wc_rhoa, wc_rhob, wc_sigaa, wc_sigab, wc_sigbb, wc_tau
            real(F64) :: rho, tau, sigma, wc, r, r_rho, r_sig, r_tau
            real(F64) :: wepsc_rhoa, wepsc_rhob, wepsc_sigaa, wepsc_sigab, wepsc_sigbb, wepsc_tau

            epsc = ZERO
            ec_rhoa = ZERO
            ec_rhob = ZERO
            ec_sigaa = ZERO
            ec_sigab = ZERO
            ec_sigbb = ZERO
            ec_taua = ZERO
            ec_taub = ZERO
            rho = rho_a + rho_b
            tau = tau_a + tau_b
            if (rho > PBE_C_RHO_THRESH .and. tau > TPSS_C_TAU_THRESH) then
                  call revpkzb_c(wepsc, wc_rhoa, wc_rhob, wc_sigaa, wc_sigab, wc_sigbb, wc_tau, &
                        rho_a, rho_b, sigma_aa, sigma_ab, sigma_bb, tau, c_coeffs, epscgga_tot, ecgga_tot_rhoa, &
                        ecgga_tot_rhob, ecgga_tot_sigaa, ecgga_tot_sigab, ecgga_tot_sigbb, epscgga_a, ecgga_a_rhoa, &
                        ecgga_a_sigaa, epscgga_b, ecgga_b_rhob, ecgga_b_sigbb, .true.)
                  sigma = sigma_aa + TWO * sigma_ab + sigma_bb
                  wc = rho * wepsc
                  !
                  ! wc = rho * wepsc
                  ! dWc/drhoa = wepsc + rho * dWepsc/drhoa
                  ! dWepsc/drhoa = (dWc/drhoa - wepsc) / rho
                  !
                  wepsc_rhoa = (wc_rhoa - wepsc) / rho
                  wepsc_rhob = (wc_rhob - wepsc) / rho
                  wepsc_sigaa = wc_sigaa / rho
                  wepsc_sigab = wc_sigab / rho
                  wepsc_sigbb = wc_sigbb / rho
                  wepsc_tau = wc_tau / rho
                  call tpss_z3_factor(r, r_rho, r_sig, r_tau, rho, sigma, tau)
                  !
                  ! Eq. 29 in Ref. 1
                  !
                  epsc = wepsc * (ONE + D_TPSS * r * wepsc)
                  ec_rhoa = wc_rhoa * (ONE + D_TPSS * r * wepsc) + wc * D_TPSS * (r_rho * wepsc + r * wepsc_rhoa)
                  ec_rhob = wc_rhob * (ONE + D_TPSS * r * wepsc) + wc * D_TPSS * (r_rho * wepsc + r * wepsc_rhob)
                  ec_sigaa = wc_sigaa * (ONE + D_TPSS * r * wepsc) + wc * D_TPSS * (r_sig * wepsc + r * wepsc_sigaa)
                  ec_sigab = wc_sigab * (ONE + D_TPSS * r * wepsc) + wc * D_TPSS * (r_sig * TWO * wepsc + r * wepsc_sigab)
                  ec_sigbb = wc_sigbb * (ONE + D_TPSS * r * wepsc) + wc * D_TPSS * (r_sig * wepsc + r * wepsc_sigbb)
                  ec_taua = wc_tau * (ONE + D_TPSS * r * wepsc) + wc * D_TPSS * (r_tau * wepsc + r * wepsc_tau)
                  ec_taub = ec_taua
            end if
                  
            if (.not. rho_a > PBE_C_FULLPOLAR_THRESH) then
                  ec_rhoa = ZERO
                  ec_sigaa = ZERO
                  ec_sigab = ZERO
                  ec_taua = ZERO
            end if

            if (.not. rho_b > PBE_C_FULLPOLAR_THRESH) then
                  ec_rhob = ZERO
                  ec_sigab = ZERO
                  ec_sigbb = ZERO
                  ec_taub = ZERO
            end if
      end subroutine u_tpss_c_template


      subroutine tpss_c_template(epsc, ec_rho, ec_sigma, ec_tau, rho, sigma, tau, c_coeffs, &
            epscgga_tot, ecgga_tot_rho, ecgga_tot_sigma, epscgga_a, ecgga_a_rhoa, ecgga_a_sigaa)
            !
            ! Compute the TPSS/revTPSS correlation energy for a spin-unpolarized densities.
            !
            ! 1. Perdew, J.P., Tao, J., Staroverov, V.N., Scuseria, G.E.,
            !    J. Chem. Phys. 120, 6898 (2004); doi: 10.1063/1.1665298
            !            
            real(F64), intent(out) :: epsc
            real(F64), intent(out) :: ec_rho
            real(F64), intent(out) :: ec_sigma
            real(F64), intent(out) :: ec_tau
            real(F64), intent(in)  :: rho
            real(F64), intent(in)  :: sigma
            real(F64), intent(in)  :: tau
            real(F64), dimension(4), intent(in) :: c_coeffs
            real(F64), intent(in) :: epscgga_tot, ecgga_tot_rho, ecgga_tot_sigma
            real(F64), intent(in) :: epscgga_a, ecgga_a_rhoa, ecgga_a_sigaa

            real(F64) :: wepsc, wc_rhoa, wc_rhob, wc_sigaa, wc_sigab, wc_sigbb, wc_tau
            real(F64) :: wc, r, r_rho, r_sig, r_tau, ec_sigaa, ec_sigab
            real(F64) :: wepsc_rhoa, wepsc_sigaa, wepsc_sigab, wepsc_tau
            real(F64) :: ecgga_tot_sigaa, ecgga_tot_sigab, ecgga_tot_sigbb
            
            if (rho > PBE_C_RHO_THRESH .and. tau > TPSS_C_TAU_THRESH) then
                  ecgga_tot_sigaa = ecgga_tot_sigma
                  ecgga_tot_sigab = ecgga_tot_sigma * TWO
                  ecgga_tot_sigbb = ecgga_tot_sigma
                  call revpkzb_c(wepsc, wc_rhoa, wc_rhob, wc_sigaa, wc_sigab, wc_sigbb, wc_tau, &
                        rho/TWO, rho/TWO, sigma/FOUR, sigma/FOUR, sigma/FOUR, tau, c_coeffs, epscgga_tot, &
                        ecgga_tot_rho, ecgga_tot_rho, ecgga_tot_sigaa, ecgga_tot_sigab, ecgga_tot_sigbb, &
                        epscgga_a, ecgga_a_rhoa, ecgga_a_sigaa, epscgga_a, ecgga_a_rhoa, ecgga_a_sigaa, .false.)
                  wc = rho * wepsc
                  !
                  ! wc = rho * wepsc
                  ! dWc/drhoa = wepsc + rho * dWepsc/drhoa
                  ! dWepsc/drhoa = (dWc/drhoa - wepsc) / rho
                  !
                  wepsc_rhoa = (wc_rhoa - wepsc) / rho
                  wepsc_sigaa = wc_sigaa / rho
                  wepsc_sigab = wc_sigab / rho
                  wepsc_tau = wc_tau / rho
                  call tpss_z3_factor(r, r_rho, r_sig, r_tau, rho, sigma, tau)
                  !
                  ! Eq. 29 in Ref. 1
                  !                  
                  epsc = wepsc * (ONE + D_TPSS * r * wepsc)
                  ec_rho = wc_rhoa * (ONE + D_TPSS * r * wepsc) + wc * D_TPSS * (r_rho * wepsc + r * wepsc_rhoa)
                  ec_sigaa = wc_sigaa * (ONE + D_TPSS * r * wepsc) + wc * D_TPSS * (r_sig * wepsc + r * wepsc_sigaa)
                  ec_sigab = wc_sigab * (ONE + D_TPSS * r * wepsc) + wc * D_TPSS * (r_sig * TWO * wepsc + r * wepsc_sigab)
                  ec_sigma = (ONE/TWO) * ec_sigaa + (ONE/FOUR) * ec_sigab
                  ec_tau = wc_tau * (ONE + D_TPSS * r * wepsc) + wc * D_TPSS * (r_tau * wepsc + r * wepsc_tau)
            else
                  epsc = ZERO
                  ec_rho = ZERO
                  ec_sigma = ZERO
                  ec_tau = ZERO
            end if
      end subroutine tpss_c_template


      subroutine tpss_c_function(c, c_zeta, c_xi2, zeta, xi2, c_coeffs)
            !
            ! Compute the function C(zeta,xi) defined in Eq. 33 of Ref. 1
            ! (with arbitrary numerical coefficients).
            !
            ! C(zeta,xi2) and its derivatives vanish for fully spin-polarized
            ! systems.
            !
            ! This subroutine is verified with Mathematica.
            !
            ! 1. Perdew, J.P., Tao, Jianmin, Staroverov, V.N., Scuseria, G.E.,
            !    J. Chem. Pnys. 120, 6898 (2004); doi: 10.1063/1.1665298
            !
            real(F64), intent(out)               :: c
            real(F64), intent(out)               :: c_zeta
            real(F64), intent(out)               :: c_xi2
            real(F64), intent(in)                :: zeta
            real(F64), intent(in)                :: xi2
            real(F64), dimension(4), intent(in)  :: c_coeffs

            real(F64) :: cz0, cz0_zeta, a, a_zeta, b, b_zeta, t, t_zeta, t_xi2, aa, bb
            real(F64), parameter :: zeta_eps = 1.0E-15_F64

            if (abs(zeta) >= ONE - zeta_eps) then
                  c = ZERO
                  c_xi2 = ZERO
                  c_zeta = ZERO
            else                  
                  cz0 = c_coeffs(1) + c_coeffs(2) * zeta**2 + c_coeffs(3) * zeta**4 + c_coeffs(4) * zeta**6
                  cz0_zeta = TWO*c_coeffs(2) * zeta + FOUR*c_coeffs(3) * zeta**3 + SIX*c_coeffs(4) * zeta**5
                  aa = ONE + zeta
                  bb = ONE - zeta
                  a = ONE / aa**(FOUR/THREE)
                  a_zeta = (-FOUR/THREE) * a / aa
                  b = ONE / bb**(FOUR/THREE)
                  b_zeta = (FOUR/THREE) * b / bb
                  t = ONE + xi2 * (a + b) / TWO
                  t_xi2 = (a + b) / TWO
                  t_zeta = xi2 * (a_zeta + b_zeta) / TWO
                  c = cz0 / t**4
                  c_xi2 = -FOUR * c / t * t_xi2
                  c_zeta = cz0_zeta / t**4 - FOUR * c / t * t_zeta
            end if
      end subroutine tpss_c_function


      subroutine revpkzb_c(epsc, ec_rhoa, ec_rhob, ec_sigaa, ec_sigab, ec_sigbb, ec_tau, &
            rho_a, rho_b, sigma_aa, sigma_ab, sigma_bb, tau_tot, c_coeffs, epscgga_tot, ecgga_tot_rhoa, &
            ecgga_tot_rhob, ecgga_tot_sigaa, ecgga_tot_sigab, ecgga_tot_sigbb, epscgga_a, ecgga_a_rhoa, &
            ecgga_a_sigaa, epscgga_b, ecgga_b_rhob, ecgga_b_sigbb, polarized)
            !
            ! Compute the revPKZB correlation energy defined in Eq. 25 of Ref. 1 for a given GGA correlation
            ! input. This subroutine can be used to compute: 1) the TPSS and revTPSS correlation,
            ! 2) the Vc(lambda->Inf) limit of the TPSS/revTPSS correlation energy (Eq. 38 in Ref. 1).
            !
            ! The total alpha+beta kinetic energy density (tau_tot) passed to this subroutine
            ! is defined as follows:
            ! tau_tot = tau_a + tau_b = sum_k Grad(Psi_{k alpha}) * Grad(Psi_{k alpha}) 
            ! + sum_l Grad(Psi_{l beta}) * Grad(Psi_{l beta}).
            ! Note that tau does not contain the factor 1/2.
            ! 
            ! This subroutine is checked with Mathematica.
            !
            ! 1. Perdew, J.P., Tao, Jianmin, Staroverov, V.N., Scuseria, G.E.,
            !    J. Chem. Phys. 120, 6898 (2004); doi: 10.1063/1.1665298
            !
            real(F64), intent(out)              :: epsc
            real(F64), intent(out)              :: ec_rhoa
            real(F64), intent(out)              :: ec_rhob
            real(F64), intent(out)              :: ec_sigaa
            real(F64), intent(out)              :: ec_sigab
            real(F64), intent(out)              :: ec_sigbb
            real(F64), intent(out)              :: ec_tau
            real(F64), intent(in)               :: rho_a
            real(F64), intent(in)               :: rho_b
            real(F64), intent(in)               :: sigma_aa
            real(F64), intent(in)               :: sigma_ab
            real(F64), intent(in)               :: sigma_bb
            real(F64), intent(in)               :: tau_tot
            real(F64), dimension(4), intent(in) :: c_coeffs
            real(F64), intent(in)               :: epscgga_tot
            real(F64), intent(in)               :: ecgga_tot_rhoa
            real(F64), intent(in)               :: ecgga_tot_rhob
            real(F64), intent(in)               :: ecgga_tot_sigaa
            real(F64), intent(in)               :: ecgga_tot_sigab
            real(F64), intent(in)               :: ecgga_tot_sigbb
            real(F64), intent(in)               :: epscgga_a
            real(F64), intent(in)               :: ecgga_a_rhoa
            real(F64), intent(in)               :: ecgga_a_sigaa
            real(F64), intent(in)               :: epscgga_b
            real(F64), intent(in)               :: ecgga_b_rhob
            real(F64), intent(in)               :: ecgga_b_sigbb
            logical, intent(in)                 :: polarized

            real(F64) :: zeta, zeta_rhoa, zeta_rhob
            real(F64) :: xi2, xi2_rhoa, xi2_rhob, xi2_sigaa, xi2_sigab, xi2_sigbb
            real(F64) :: t1, t2, t2_rho, delzeta2, delzeta2_rhoa, delzeta2_rhob, delzeta2_rho
            real(F64) :: delzeta2_sigaa, delzeta2_sigab, delzeta2_sigbb, r, r_rho, r_tau, r_sigma
            real(F64) :: rho, sigma, c, c_zeta, c_xi2, ecgga_tot, ecgga_a, ecgga_b, rhoafrac, rhobfrac
            real(F64) :: c_rhoa, c_rhob, c_sigaa, c_sigab, c_sigbb
            real(F64) :: f1, f1_rhoa, f1_rhob, f1_sigaa, f1_sigab, f1_sigbb, f1_tau
            real(F64) :: f2, f2_rhoa, f2_rhob, f2_sigaa, f2_sigab, f2_sigbb, f2_tau
            real(F64) :: z, z_rho, z_sig, z_tau, tau
            !
            ! Switch to the definition of tau with the 1/2 factor
            !
            tau = tau_tot / TWO
            sigma = sigma_aa + TWO * sigma_ab + sigma_bb
            rho = rho_a + rho_b
            
            if (polarized) then
                  zeta = (rho_a - rho_b) / (rho_a + rho_b)
                  zeta_rhoa = TWO * rho_b / (rho_a + rho_b)**2
                  zeta_rhob = -TWO * rho_a / (rho_a + rho_b)**2
                  !
                  ! xi**2 = (grad zeta)**2 / (2 * (3 PI**2 * rho)**(1/3))**2 (Eq. 28 in Ref. 1)
                  !
                  t1 = FOUR / rho**4
                  delzeta2 = t1 * (rho_b**2 * sigma_aa + rho_a**2 * sigma_bb - TWO * rho_a * rho_b * sigma_ab)
                  delzeta2_rho = -FOUR * delzeta2 / rho
                  delzeta2_rhoa = t1 * TWO * (rho_a * sigma_bb - rho_b * sigma_ab) + delzeta2_rho
                  delzeta2_rhob = t1 * TWO * (rho_b * sigma_aa - rho_a * sigma_ab) + delzeta2_rho
                  delzeta2_sigaa = t1 * rho_b**2
                  delzeta2_sigab = t1 * (-TWO) * rho_a * rho_b
                  delzeta2_sigbb = t1 * rho_a**2
                  t2 = ONE / ((TWO*(THREE*PI**2)**(ONE/THREE))**2 * rho**(TWO/THREE))
                  t2_rho = -(TWO/THREE) * t2 / rho
                  xi2 = delzeta2 * t2
                  xi2_rhoa = delzeta2_rhoa * t2 + delzeta2 * t2_rho
                  xi2_rhob = delzeta2_rhob * t2 + delzeta2 * t2_rho
                  xi2_sigaa = delzeta2_sigaa * t2
                  xi2_sigab = delzeta2_sigab * t2
                  xi2_sigbb = delzeta2_sigbb * t2
                  call tpss_c_function(c, c_zeta, c_xi2, zeta, xi2, c_coeffs)
                  c_rhoa = c_zeta * zeta_rhoa + c_xi2 * xi2_rhoa
                  c_rhob = c_zeta * zeta_rhob + c_xi2 * xi2_rhob 
                  c_sigaa = c_xi2 * xi2_sigaa
                  c_sigab = c_xi2 * xi2_sigab
                  c_sigbb = c_xi2 * xi2_sigbb
            else
                  c = c_coeffs(1)
                  c_rhoa = ZERO
                  c_rhob = ZERO
                  c_sigaa = ZERO
                  c_sigab = ZERO
                  c_sigbb = ZERO
            end if

            call tpss_z_factor(z, z_rho, z_sig, z_tau, rho, sigma, tau)
            r = z**2
            r_rho = TWO * z * z_rho
            r_sigma = TWO * z * z_sig
            r_tau = TWO * z * z_tau

            f1 = ONE + c * r
            f1_rhoa = c_rhoa * r + c * r_rho
            f1_rhob = c_rhob * r + c * r_rho
            f1_sigaa = c_sigaa * r + c * r_sigma
            f1_sigab = c_sigab * r + c * r_sigma * TWO
            f1_sigbb = c_sigbb * r + c * r_sigma
            f1_tau = c * r_tau

            f2 = (ONE + c) * r
            f2_rhoa = c_rhoa * r + (ONE + c) * r_rho
            f2_rhob = c_rhob * r + (ONE + c) * r_rho
            f2_sigaa = c_sigaa * r + (ONE + c) * r_sigma
            f2_sigab = c_sigab * r + (ONE + c) * r_sigma * TWO
            f2_sigbb = c_sigbb * r + (ONE + c) * r_sigma
            f2_tau = (ONE + c) * r_tau

            epsc = epscgga_tot * f1
            ecgga_tot = rho * epscgga_tot
            ec_rhoa = ecgga_tot_rhoa * f1 + ecgga_tot * f1_rhoa
            ec_rhob = ecgga_tot_rhob * f1 + ecgga_tot * f1_rhob
            ec_sigaa = ecgga_tot_sigaa * f1 + ecgga_tot * f1_sigaa
            ec_sigab = ecgga_tot_sigab * f1 + ecgga_tot * f1_sigab
            ec_sigbb = ecgga_tot_sigbb * f1 + ecgga_tot * f1_sigbb
            ec_tau = ecgga_tot * f1_tau

            if (epscgga_a > epscgga_tot) then
                  ecgga_a = rho_a * epscgga_a
                  epsc = epsc - f2 * ecgga_a / rho
                  ec_rhoa = ec_rhoa - f2_rhoa * ecgga_a - f2 * ecgga_a_rhoa
                  ec_rhob = ec_rhob - f2_rhob * ecgga_a
                  ec_sigaa = ec_sigaa - f2_sigaa * ecgga_a - f2 * ecgga_a_sigaa
                  ec_sigab = ec_sigab - f2_sigab * ecgga_a
                  ec_sigbb = ec_sigbb - f2_sigbb * ecgga_a
                  ec_tau = ec_tau - f2_tau * ecgga_a
            else
                  ecgga_a = rho_a * epscgga_tot
                  rhoafrac = rho_a / rho
                  epsc = epsc - f2 * ecgga_a / rho
                  !
                  ! d/drhoa (rhoa/rho)*ecgga = (1/rho)*ecgga - (rhoa/rho**2)ecgga 
                  ! + (rhoa/rho)*d/drhoa ecgga
                  ! = (rhob/rho**2)*ecgga + (rhoa/rho)*d/drhoa ecgga
                  !
                  ec_rhoa = ec_rhoa - f2_rhoa * ecgga_a - f2 * (rho_b/rho**2 * ecgga_tot &
                        + rhoafrac * ecgga_tot_rhoa)
                  ec_rhob = ec_rhob - f2_rhob * ecgga_a - f2 * (-rho_a/rho**2 * ecgga_tot &
                        + rhoafrac * ecgga_tot_rhob)                  
                  ec_sigaa = ec_sigaa - f2_sigaa * ecgga_a - f2 * rhoafrac * ecgga_tot_sigaa
                  ec_sigab = ec_sigab - f2_sigab * ecgga_a - f2 * rhoafrac * ecgga_tot_sigab
                  ec_sigbb = ec_sigbb - f2_sigbb * ecgga_a - f2 * rhoafrac * ecgga_tot_sigbb
                  ec_tau = ec_tau - f2_tau * ecgga_a
            end if

            if (epscgga_b > epscgga_tot) then
                  ecgga_b = rho_b * epscgga_b
                  epsc = epsc - f2 * ecgga_b / rho                  
                  ec_rhoa = ec_rhoa - f2_rhoa * ecgga_b
                  ec_rhob = ec_rhob - f2_rhob * ecgga_b - f2 * ecgga_b_rhob                  
                  ec_sigaa = ec_sigaa - f2_sigaa * ecgga_b                  
                  ec_sigab = ec_sigab - f2_sigab * ecgga_b
                  ec_sigbb = ec_sigbb - f2_sigbb * ecgga_b - f2 * ecgga_b_sigbb
                  ec_tau = ec_tau - f2_tau * ecgga_b
            else
                  ecgga_b = rho_b * epscgga_tot
                  rhobfrac = rho_b / rho
                  epsc = epsc - f2 * ecgga_b / rho
                  ec_rhoa = ec_rhoa - f2_rhoa * ecgga_b - f2 * (-rho_b/rho**2 * ecgga_tot &
                        + rhobfrac * ecgga_tot_rhoa)
                  !
                  ! d/drhob (rhob/rho)*ecgga = (1/rho)*ecgga - (rhob/rho**2)ecgga 
                  ! + (rhob/rho)*d/drhoa ecgga
                  ! = (rhoa/rho**2)*ecgga + (rhob/rho)*d/drhoa ecgga
                  !
                  ec_rhob = ec_rhob - f2_rhob * ecgga_b - f2 * (rho_a/rho**2 * ecgga_tot &
                        + rhobfrac * ecgga_tot_rhob)
                  ec_sigaa = ec_sigaa - f2_sigaa * ecgga_b - f2 * rhobfrac * ecgga_tot_sigaa
                  ec_sigab = ec_sigab - f2_sigab * ecgga_b - f2 * rhobfrac * ecgga_tot_sigab
                  ec_sigbb = ec_sigbb - f2_sigbb * ecgga_b - f2 * rhobfrac * ecgga_tot_sigbb
                  ec_tau = ec_tau - f2_tau * ecgga_b
            end if
            !
            ! Switch back to the definition of tau without the 1/2 factor
            !
            ec_tau = ec_tau / TWO
      end subroutine revpkzb_c
end module tpss_c_energy
