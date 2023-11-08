module tpss_xc_auxiliary
      use arithmetic
      use math_constants
      use tpss_x_energy
      use tpss_c_energy
      use pbe_xc_auxiliary

      implicit none

contains

      subroutine u_tpss_vxc_inf(vxcinf, rho_a, rho_b, sigma_aa, sigma_ab, sigma_bb, tau_a, tau_b)
            !
            ! Compute the adiabatic connection integrand, Vxc, in the limit of infinite
            ! interaction strength (lambda->inf), using the TPSS expression for
            ! the exchange-correlation energy. (See Eq. 38 in Ref. 2.)
            !
            ! The values of vxcinf are checked against Table 1 in Ref. 2.
            ! All the required checks for vanishing densities are done inside this subroutine.
            !
            ! 1. Seidl, M., Perdew, J.P., Kurth, S., Phys. Rev. A 62, 012502 (2000);
            !    doi: 10.1103/PhysRevA.62.012502
            ! 2. Perdew, J.P., Tao, J., Staroverov, V.N., Scuseria, G.E.,
            !    J. Chem. Phys. 120, 6898 (2004); doi: 10.1063/1.1665298
            !            
            real(F64), intent(out) :: vxcinf
            real(F64), intent(in)  :: rho_a
            real(F64), intent(in)  :: rho_b
            real(F64), intent(in)  :: sigma_aa
            real(F64), intent(in)  :: sigma_ab
            real(F64), intent(in)  :: sigma_bb
            real(F64), intent(in)  :: tau_a
            real(F64), intent(in)  :: tau_b

            real(F64) :: epscinf, vcinf_rhoa, vcinf_rhob, vcinf_sigaa, vcinf_sigab, vcinf_sigbb
            real(F64) :: vcinf_taua, vcinf_taub, epsxa, epsxb, epsx, ex_rho, ex_sigma, ex_tau

            call u_tpss_vc_inf(epscinf, vcinf_rhoa, vcinf_rhob, vcinf_sigaa, vcinf_sigab, vcinf_sigbb, &
                  vcinf_taua, vcinf_taub, rho_a, rho_b, sigma_aa, sigma_ab, sigma_bb, tau_a, tau_b)

            epsx = ZERO
            if (TWO*rho_a > TPSS_X_RHO_THRESH .and. TWO*tau_a > TPSS_X_TAU_THRESH) then
                  call tpss_x(epsxa, ex_rho, ex_sigma, ex_tau, TWO*rho_a, FOUR*sigma_aa, TWO*tau_a)
                  epsx = epsx + (rho_a / (rho_a + rho_b)) * epsxa
            end if

            if (TWO*rho_b > TPSS_X_RHO_THRESH .and. TWO*tau_b > TPSS_X_TAU_THRESH) then
                  call tpss_x(epsxb, ex_rho, ex_sigma, ex_tau, TWO*rho_b, FOUR*sigma_bb, TWO*tau_b)
                  epsx = epsx + (rho_b / (rho_a + rho_b)) * epsxb
            end if

            vxcinf = (rho_a + rho_b) * (epsx + epscinf)
      end subroutine u_tpss_vxc_inf


      subroutine u_tpss_vc_inf(epscinf, vcinf_rhoa, vcinf_rhob, vcinf_sigaa, vcinf_sigab, vcinf_sigbb, &
            vcinf_taua, vcinf_taub, rho_a, rho_b, sigma_aa, sigma_ab, sigma_bb, tau_a, tau_b)
            !
            ! Compute the adiabatic connection integrand, Vc, in the limit of infinite
            ! interaction strength (lambda->inf), using the TPSS expression for the correlation
            ! energy. (See Eq. 38 in Ref. 2.)
            !
            ! All the required checks for vanishing densities are done inside this subroutine.
            !
            ! 1. Seidl, M., Perdew, J.P., Kurth, S., Phys. Rev. A 62, 012502 (2000);
            !    doi: 10.1103/PhysRevA.62.012502
            ! 2. Perdew, J.P., Tao, J., Staroverov, V.N., Scuseria, G.E.,
            !    J. Chem. Phys. 120, 6898 (2004); doi: 10.1063/1.1665298
            !            
            real(F64), intent(out) :: epscinf
            real(F64), intent(out) :: vcinf_rhoa
            real(F64), intent(out) :: vcinf_rhob
            real(F64), intent(out) :: vcinf_sigaa
            real(F64), intent(out) :: vcinf_sigab
            real(F64), intent(out) :: vcinf_sigbb
            real(F64), intent(out) :: vcinf_taua
            real(F64), intent(out) :: vcinf_taub
            real(F64), intent(in)  :: rho_a
            real(F64), intent(in)  :: rho_b
            real(F64), intent(in)  :: sigma_aa
            real(F64), intent(in)  :: sigma_ab
            real(F64), intent(in)  :: sigma_bb
            real(F64), intent(in)  :: tau_a
            real(F64), intent(in)  :: tau_b

            real(F64) :: epscgga_tot, ecgga_tot_rhoa, ecgga_tot_rhob, ecgga_tot_sigma
            real(F64) :: ecgga_tot_sigaa, ecgga_tot_sigab, ecgga_tot_sigbb
            real(F64) :: epscgga_a, ecgga_a_rhoa, ecgga_a_sigaa
            real(F64) :: epscgga_b, ecgga_b_rhob, ecgga_b_sigbb
            real(F64) :: tau, sigma, vcinf_tau

            tau = tau_a + tau_b
            if ((rho_a+rho_b) > PBE_C_RHO_THRESH .and. rho_a > PBE_C_FULLPOLAR_THRESH &
                  .and. rho_b > PBE_C_FULLPOLAR_THRESH .and. tau > TPSS_C_TAU_THRESH) then
                  sigma = sigma_aa + TWO * sigma_ab + sigma_bb
                  call pbe_vc_inf_polar(epscgga_tot, ecgga_tot_rhoa, ecgga_tot_rhob, ecgga_tot_sigma, &
                        PBE_BETA_EXTRADIGITS, rho_a, rho_b, sigma)
                  ecgga_tot_sigaa = ecgga_tot_sigma
                  ecgga_tot_sigab = ecgga_tot_sigma * TWO
                  ecgga_tot_sigbb = ecgga_tot_sigma
                  call pbe_vc_inf_fullpolar(epscgga_a, ecgga_a_rhoa, ecgga_a_sigaa, PBE_BETA_EXTRADIGITS, &
                        rho_a, sigma_aa)
                  call pbe_vc_inf_fullpolar(epscgga_b, ecgga_b_rhob, ecgga_b_sigbb, PBE_BETA_EXTRADIGITS, &
                        rho_b, sigma_bb)
                  call revpkzb_c(epscinf, vcinf_rhoa, vcinf_rhob, vcinf_sigaa, vcinf_sigab, vcinf_sigbb, vcinf_tau, &
                        rho_a, rho_b, sigma_aa, sigma_ab, sigma_bb, tau, TPSS_C_COEFFS, epscgga_tot, ecgga_tot_rhoa, &
                        ecgga_tot_rhob, ecgga_tot_sigaa, ecgga_tot_sigab, ecgga_tot_sigbb, epscgga_a, ecgga_a_rhoa, &
                        ecgga_a_sigaa, epscgga_b, ecgga_b_rhob, ecgga_b_sigbb, .true.)
                  vcinf_taua = vcinf_tau
                  vcinf_taub = vcinf_tau

            else if ((rho_a+rho_b) > PBE_C_RHO_THRESH .and. rho_a > PBE_C_FULLPOLAR_THRESH &
                  .and. tau > TPSS_C_TAU_THRESH) then
                  call pbe_vc_inf_fullpolar(epscgga_a, ecgga_a_rhoa, ecgga_a_sigaa, PBE_BETA_EXTRADIGITS, &
                        rho_a, sigma_aa)
                  epscgga_tot = epscgga_a
                  ecgga_tot_rhoa = ecgga_a_rhoa
                  ecgga_tot_rhob = ZERO
                  ecgga_tot_sigaa = ecgga_a_sigaa
                  ecgga_tot_sigab = ZERO
                  ecgga_tot_sigbb = ZERO
                  epscgga_b = ZERO
                  ecgga_b_rhob = ZERO
                  ecgga_b_sigbb = ZERO
                  call revpkzb_c(epscinf, vcinf_rhoa, vcinf_rhob, vcinf_sigaa, vcinf_sigab, vcinf_sigbb, vcinf_tau, &
                        rho_a, rho_b, sigma_aa, sigma_ab, sigma_bb, tau, TPSS_C_COEFFS, epscgga_tot, ecgga_tot_rhoa, &
                        ecgga_tot_rhob, ecgga_tot_sigaa, ecgga_tot_sigab, ecgga_tot_sigbb, epscgga_a, ecgga_a_rhoa, &
                        ecgga_a_sigaa, epscgga_b, ecgga_b_rhob, ecgga_b_sigbb, .true.)
                  vcinf_rhob = ZERO
                  vcinf_sigab = ZERO
                  vcinf_sigbb = ZERO
                  vcinf_taua = vcinf_tau
                  vcinf_taub = ZERO

            else if ((rho_a+rho_b) > PBE_C_RHO_THRESH .and. rho_b > PBE_C_FULLPOLAR_THRESH &
                  .and. tau > TPSS_C_TAU_THRESH) then
                  call pbe_vc_inf_fullpolar(epscgga_b, ecgga_b_rhob, ecgga_b_sigbb, PBE_BETA_EXTRADIGITS, &
                        rho_b, sigma_bb)
                  epscgga_tot = epscgga_b
                  ecgga_tot_rhoa = ZERO
                  ecgga_tot_rhob = ecgga_b_rhob
                  ecgga_tot_sigaa = ZERO
                  ecgga_tot_sigab = ZERO
                  ecgga_tot_sigbb = ecgga_b_sigbb                              
                  epscgga_a = ZERO
                  ecgga_a_rhoa = ZERO
                  ecgga_a_sigaa = ZERO
                  call revpkzb_c(epscinf, vcinf_rhoa, vcinf_rhob, vcinf_sigaa, vcinf_sigab, vcinf_sigbb, vcinf_tau, &
                        rho_a, rho_b, sigma_aa, sigma_ab, sigma_bb, tau, TPSS_C_COEFFS, epscgga_tot, ecgga_tot_rhoa, &
                        ecgga_tot_rhob, ecgga_tot_sigaa, ecgga_tot_sigab, ecgga_tot_sigbb, epscgga_a, ecgga_a_rhoa, &
                        ecgga_a_sigaa, epscgga_b, ecgga_b_rhob, ecgga_b_sigbb, .true.)
                  vcinf_rhoa = ZERO
                  vcinf_sigaa = ZERO
                  vcinf_sigab = ZERO
                  vcinf_taua = ZERO
                  vcinf_taub = vcinf_tau
                  
            else
                  epscinf = ZERO
                  vcinf_rhoa = ZERO
                  vcinf_rhob = ZERO
                  vcinf_sigaa = ZERO
                  vcinf_sigab = ZERO
                  vcinf_sigbb = ZERO
                  vcinf_taua = ZERO
                  vcinf_taub = ZERO
            end if
      end subroutine u_tpss_vc_inf
end module tpss_xc_auxiliary
