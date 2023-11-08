module pbe_c_energy
      use arithmetic
      use math_constants

      implicit none
      !
      ! The original beta parameter defined below Eq. 4 in Phys. Rev. Lett. 77, 3865 (1996)
      !
      real(F64), parameter :: PBE_BETA_DEFAULT = 0.066725_F64
      !
      ! The original beta parameter defined below Eq. 4 in Phys. Rev. Lett. 77, 3865 (1996)
      ! supplied with extra digits of accuracy from K. Burke's Fortran subroutine
      !
      real(F64), parameter :: PBE_BETA_EXTRADIGITS = 0.06672455060314922_F64
      !
      ! rs->Infinity limit of the revTPSS parametrization of beta (Eq. 3 in Phys. Rev. Lett. 103, 026403 (2009))
      ! lim_{rs->Inf} beta(rs) = lim_{rs->Inf} 0.066725*(1 + 0.1 rs)/(1 + 0.1778 rs) = 0.0375281214848144
      !
      real(F64), parameter :: PBE_BETA_REVTPSS_LIMIT = 0.0375281214848144_F64
      !
      ! The beta parameter for PBEsol defined below the caption of Fig. 1 in Phys. Rev. Lett. 100, 136406 (2008);
      ! doi: 10.1103/PhysRevLett.100.136406
      !
      real(F64), parameter :: PBEsol_BETA = 0.046_F64
      real(F64), parameter :: PBE_C_RHO_THRESH = 1.0E-10_F64
      real(F64), parameter :: PBE_C_FULLPOLAR_THRESH = 1.0E-13_F64

contains

      subroutine pbe_c(epsc, ec_rho, ec_sigma, rho, sigma)
            !
            ! PBE correlation for closed-shell densities.
            ! The set of admissible arguments includes rho and sigma which are exactly zero.
            !
            ! 1. Perdew, J.P., Burke, K., Ernzerhof, M., Phys. Rev. Lett. 77, 3865 (1996);
            !    doi: 10.1103/PhysRevLett.77.3865
            !
            real(F64), intent(out) :: epsc
            real(F64), intent(out) :: ec_rho
            real(F64), intent(out) :: ec_sigma
            real(F64), intent(in)  :: rho
            real(F64), intent(in)  :: sigma

            call pbe_c_template_unpolar(epsc, ec_rho, ec_sigma, rho, sigma, PBE_BETA_EXTRADIGITS, ZERO)
      end subroutine pbe_c


      subroutine pbesol_c(epsc, ec_rho, ec_sigma, rho, sigma)
            !
            ! PBEsol correlation for closed-shell densities.
            ! The set of admissible arguments includes rho and sigma which are exactly zero.
            !
            ! 1. Phys. Rev. Lett. 100, 136406 (2008); doi: 10.1103/PhysRevLett.100.136406
            !
            real(F64), intent(out) :: epsc
            real(F64), intent(out) :: ec_rho
            real(F64), intent(out) :: ec_sigma
            real(F64), intent(in)  :: rho
            real(F64), intent(in)  :: sigma

            call pbe_c_template_unpolar(epsc, ec_rho, ec_sigma, rho, sigma, PBEsol_BETA, ZERO)
      end subroutine pbesol_c


      subroutine mvs_c(epsc, ec_rho, ec_sigma, rho, sigma)
            !
            ! Correlation functional of Sun et al. (Refs. 1 and 2) for closed-shell densities.
            ! The set of admissible arguments includes rho and sigma which are exactly zero.
            !
            ! 1. Sun, J., Perdew, J.P., Ruzsinszky, A., PNAS 112, 685 (2015);
            !    doi: 10.1073/pnas.1423145112
            ! 2. Sun, J., Xiao, B., Ruzsinszky, A., J. Chem. Phys. 137, 051101 (2012); 
            !    doi: 10.1063/1.4742312
            !
            real(F64), intent(out) :: epsc
            real(F64), intent(out) :: ec_rho
            real(F64), intent(out) :: ec_sigma
            real(F64), intent(in)  :: rho
            real(F64), intent(in)  :: sigma

            real(F64) :: beta, beta_rho

            call beta_revtpss(beta, beta_rho, rho)
            call pbe_c_template_unpolar(epsc, ec_rho, ec_sigma, rho, sigma, beta, beta_rho)
      end subroutine mvs_c


      subroutine u_pbe_c(epsc, ec_rhoa, ec_rhob, ec_sigaa, ec_sigab, ec_sigbb, rho_a, rho_b, sigma_aa, sigma_ab, sigma_bb)
            !
            ! PBE correlation for open-shell densities.
            ! The set of admissible arguments includes rho and sigma which are exactly zero.
            !
            ! 1. Perdew, J.P., Burke, K., Ernzerhof, M., Phys. Rev. Lett. 77, 3865 (1996);
            !    doi: 10.1103/PhysRevLett.77.3865
            !
            real(F64), intent(out) :: epsc
            real(F64), intent(out) :: ec_rhoa
            real(F64), intent(out) :: ec_rhob
            real(F64), intent(out) :: ec_sigaa
            real(F64), intent(out) :: ec_sigab
            real(F64), intent(out) :: ec_sigbb
            real(F64), intent(in)  :: rho_a
            real(F64), intent(in)  :: rho_b
            real(F64), intent(in)  :: sigma_aa
            real(F64), intent(in)  :: sigma_ab
            real(F64), intent(in)  :: sigma_bb

            real(F64) :: sigma

            sigma = sigma_aa + TWO * sigma_ab + sigma_bb
            call pbe_c_template_polar(epsc, ec_rhoa, ec_rhob, ec_sigaa, ec_sigab, ec_sigbb, rho_a, rho_b, sigma, &
                  PBE_BETA_EXTRADIGITS, ZERO)
      end subroutine u_pbe_c


      subroutine u_pbesol_c(epsc, ec_rhoa, ec_rhob, ec_sigaa, ec_sigab, ec_sigbb, rho_a, rho_b, sigma_aa, sigma_ab, sigma_bb)
            !
            ! PBEsol correlation for open-shell densities.
            ! The set of admissible arguments includes rho and sigma which are exactly zero.
            !
            ! 1. Phys. Rev. Lett. 100, 136406 (2008); doi: 10.1103/PhysRevLett.100.136406
            !
            real(F64), intent(out) :: epsc
            real(F64), intent(out) :: ec_rhoa
            real(F64), intent(out) :: ec_rhob
            real(F64), intent(out) :: ec_sigaa
            real(F64), intent(out) :: ec_sigab
            real(F64), intent(out) :: ec_sigbb
            real(F64), intent(in)  :: rho_a
            real(F64), intent(in)  :: rho_b
            real(F64), intent(in)  :: sigma_aa
            real(F64), intent(in)  :: sigma_ab
            real(F64), intent(in)  :: sigma_bb

            real(F64) :: sigma

            sigma = sigma_aa + TWO * sigma_ab + sigma_bb
            call pbe_c_template_polar(epsc, ec_rhoa, ec_rhob, ec_sigaa, ec_sigab, ec_sigbb, rho_a, rho_b, sigma, &
                  PBEsol_BETA, ZERO)
      end subroutine u_pbesol_c


      subroutine u_mvs_c(epsc, ec_rhoa, ec_rhob, ec_sigaa, ec_sigab, ec_sigbb, rho_a, rho_b, sigma_aa, sigma_ab, sigma_bb)
            !
            ! Correlation functional of Sun et al. (Refs. 1 and 2) for open-shell densities.
            ! The set of admissible arguments includes rho and sigma which are exactly zero.
            ! Fully spin-polarized densities are accepted.
            !
            ! 1. Sun, J., Perdew, J.P., Ruzsinszky, A., PNAS 112, 685 (2015);
            !    doi: 10.1073/pnas.1423145112
            ! 2. Sun, J., Xiao, B., Ruzsinszky, A., J. Chem. Phys. 137, 051101 (2012); 
            !    doi: 10.1063/1.4742312
            !
            real(F64), intent(out) :: epsc
            real(F64), intent(out) :: ec_rhoa
            real(F64), intent(out) :: ec_rhob
            real(F64), intent(out) :: ec_sigaa
            real(F64), intent(out) :: ec_sigab
            real(F64), intent(out) :: ec_sigbb
            real(F64), intent(in)  :: rho_a
            real(F64), intent(in)  :: rho_b
            real(F64), intent(in)  :: sigma_aa
            real(F64), intent(in)  :: sigma_ab
            real(F64), intent(in)  :: sigma_bb

            real(F64) :: sigma, beta, beta_rho
            
            call beta_revtpss(beta, beta_rho, rho_a+rho_b)
            sigma = sigma_aa + TWO * sigma_ab + sigma_bb
            call pbe_c_template_polar(epsc, ec_rhoa, ec_rhob, ec_sigaa, ec_sigab, ec_sigbb, rho_a, &
                  rho_b, sigma, beta, beta_rho)
      end subroutine u_mvs_c


      subroutine beta_revtpss(beta, beta_rho, rho)
            !
            ! Parametrization of the gradient coefficient (beta) used in revTPSS,
            ! MVS (meta-GGA made very simple), and SCAN:
            ! beta(rs) = 0.066725 * (1 + 0.1 * rs) / (1 + 0.1778 * rs)
            ! (Phys. Rev. Lett. 103, 026403 (2009)).
            ! A zero density is admissible.
            !
            real(F64), intent(out) :: beta
            real(F64), intent(out) :: beta_rho
            real(F64), intent(in)  :: rho

            real(F64) :: rho13
            real(F64), parameter :: eps = 1.0E-300_F64
            real(F64), parameter :: a0 = 0.00413928865052624661_F64
            real(F64), parameter :: a1 = 0.066725_F64
            real(F64), parameter :: b0 = 0.110298317281913323_F64
            real(F64), parameter :: c0 = 0.00107345552336980662_F64
            !
            ! Add a small positive number to rho to avoid division by zero in dbeta/drho
            !
            rho13 = (rho + eps)**(ONE/THREE)
            beta = (a0 + a1 * rho13) / (b0 + rho13)
            beta_rho = c0 / ((b0 + rho13)**2 * rho13**2)
      end subroutine beta_revtpss


      subroutine pbe_c_template_polar(epsc, ec_rhoa, ec_rhob, ec_sigaa, ec_sigab, ec_sigbb, rho_a, &
            rho_b, sigma, beta, beta_rho)
            real(F64), intent(out) :: epsc
            real(F64), intent(out) :: ec_rhoa
            real(F64), intent(out) :: ec_rhob
            real(F64), intent(out) :: ec_sigaa
            real(F64), intent(out) :: ec_sigab
            real(F64), intent(out) :: ec_sigbb
            real(F64), intent(in)  :: rho_a
            real(F64), intent(in)  :: rho_b
            real(F64), intent(in)  :: sigma
            real(F64), intent(in)  :: beta
            real(F64), intent(in)  :: beta_rho

            real(F64) :: epscUEG, ecUEG_rho, ecUEG_zeta, rs, y1, y2, zeta, phi, phi_zeta, rho
            real(F64) :: epscUEG_rs, rs_rho, ec_sigma, ec_beta

            if ((rho_a+rho_b) > PBE_C_RHO_THRESH .and. rho_a > PBE_C_FULLPOLAR_THRESH &
                  .and. rho_b > PBE_C_FULLPOLAR_THRESH) then
                  rho = rho_a + rho_b
                  rs = (THREE/(FOUR*PI))**(ONE/THREE) / rho**(ONE/THREE)
                  zeta = (rho_a - rho_b) / rho
                  call pw92_c(epscUEG, ecUEG_rho, ecUEG_zeta, rho, rs, zeta)
                  y1 = (ONE + zeta)**(ONE/THREE)
                  y2 = (ONE - zeta)**(ONE/THREE)
                  phi = (y1**2 + y2**2) / TWO
                  phi_zeta = (ONE / y1 - ONE / y2) / THREE
                  call pbe_c_template_general(epsc, ec_rhoa, ec_rhob, ec_sigma, ec_beta, rho_a, rho_b, sigma, &
                        beta, phi, phi_zeta, epscUEG, ecUEG_rho, ecUEG_zeta)
                  ec_sigaa = ec_sigma
                  ec_sigab = ec_sigma * TWO
                  ec_sigbb = ec_sigma
                  ec_rhoa = ec_rhoa + ec_beta * beta_rho
                  ec_rhob = ec_rhob + ec_beta * beta_rho
            else if ((rho_a+rho_b) > PBE_C_RHO_THRESH) then
                  !
                  ! One of spin-densities is below PBE_C_FULLPOLAR_THRESH
                  !
                  ! Special case of fully-polarized densities is needed to avoid
                  ! division by zero in dphi/dzeta. All derivatives corresponding
                  ! to the vanishing density are set to zero.
                  !
                  rho = rho_a + rho_b
                  rs = (THREE/(FOUR*PI))**(ONE/THREE) / rho**(ONE/THREE)
                  call pw92_c_fullpolar(epscUEG, epscUEG_rs, rs)
                  rs_rho = (-ONE/THREE) * rs / rho
                  !
                  ! EcUEG = rho * EpscUEG
                  ! dEcUEG/drho = EpscUEG + rho * dEpscUEG/drho
                  !
                  ecUEG_rho = epscUEG + rho * epscUEG_rs * rs_rho
                  ecUEG_zeta = ZERO
                  phi = ONE / TWO**(ONE/THREE)
                  phi_zeta = ZERO       
                  call pbe_c_template_general(epsc, ec_rhoa, ec_rhob, ec_sigma, ec_beta, rho_a, rho_b, sigma, &
                        beta, phi, phi_zeta, epscUEG, ecUEG_rho, ecUEG_zeta)
                  if (rho_a > PBE_C_FULLPOLAR_THRESH) then
                        ec_rhoa = ec_rhoa + ec_beta * beta_rho
                        ec_rhob = ZERO
                        ec_sigaa = ec_sigma
                        ec_sigbb = ZERO
                  else
                        ec_rhoa = ZERO
                        ec_rhob = ec_rhob + ec_beta * beta_rho
                        ec_sigaa = ZERO
                        ec_sigbb = ec_sigma
                  end if
                  ec_sigab = ZERO
            else
                  epsc = ZERO
                  ec_rhoa = ZERO
                  ec_rhob = ZERO
                  ec_sigaa = ZERO
                  ec_sigab = ZERO
                  ec_sigbb = ZERO
            end if
      end subroutine pbe_c_template_polar

      
      subroutine pbe_c_template_unpolar(epsc, ec_rho, ec_sigma, rho, sigma, beta, beta_rho)
            real(F64), intent(out) :: epsc
            real(F64), intent(out) :: ec_rho
            real(F64), intent(out) :: ec_sigma
            real(F64), intent(in)  :: rho
            real(F64), intent(in)  :: sigma
            real(F64), intent(in)  :: beta
            real(F64), intent(in)  :: beta_rho

            real(F64) :: epscUEG, ecUEG_rho, ecUEG_zeta, rs, rho_a, rho_b
            real(F64) :: epscUEG_rs, rs_rho, phi, phi_zeta, ec_beta, ec_rhoa, ec_rhob

            if (rho > PBE_C_RHO_THRESH) then
                  rs = (THREE/(FOUR*PI))**(ONE/THREE) / rho**(ONE/THREE)
                  call pw92_c_unpolar(epscUEG, epscUEG_rs, rs)
                  rs_rho = (-ONE/THREE) * rs / rho
                  !
                  ! EcUEG = rho * EpscUEG
                  ! dEcUEG/drho = EpscUEG + rho * dEpscUEG/drho
                  !
                  ecUEG_rho = epscUEG + rho * epscUEG_rs * rs_rho
                  ecUEG_zeta = ZERO
                  phi = ONE
                  phi_zeta = ZERO
                  rho_a = rho / TWO
                  rho_b = rho / TWO
                  call pbe_c_template_general(epsc, ec_rhoa, ec_rhob, ec_sigma, ec_beta, rho_a, rho_b, sigma, &
                        beta, phi, phi_zeta, epscUEG, ecUEG_rho, ecUEG_zeta)
                  ec_rho = ec_rhoa + ec_beta * beta_rho
            else
                  epsc = ZERO
                  ec_rho = ZERO
                  ec_sigma = ZERO
            end if
      end subroutine pbe_c_template_unpolar


      subroutine pbe_c_template_general(epsc, ec_rhoa, ec_rhob, ec_sigma, ec_beta, rho_a, rho_b, sigma, &
            beta, phi, phi_zeta, epscUEG, ecUEG_rho, ecUEG_zeta)
            !
            ! Compute the PBE correlation energy for spin-polarized densities, for an arbitrary
            ! value of beta. The derivative with respect to beta is computed for functionals
            ! which use rs-dependent parametrization of beta.
            !
            ! 1. Perdew, J.P., Burke, K., Ernzerhof, M., Generalized Gradient Approximation Made Simple,
            !    Phys. Rev. Lett. 18, 3865 (1996); doi: 10.1103/PhysRevLett.77.3865
            !
            real(F64), intent(out) :: epsc
            real(F64), intent(out) :: ec_rhoa
            real(F64), intent(out) :: ec_rhob
            real(F64), intent(out) :: ec_sigma
            real(F64), intent(out) :: ec_beta
            real(F64), intent(in)  :: rho_a
            real(F64), intent(in)  :: rho_b
            real(F64), intent(in)  :: sigma
            real(F64), intent(in)  :: beta
            real(F64), intent(in)  :: phi
            real(F64), intent(in)  :: phi_zeta
            real(F64), intent(in)  :: epscUEG
            real(F64), intent(in)  :: ecUEG_rho
            real(F64), intent(in)  :: ecUEG_zeta
            
            real(F64) :: zeta, zeta_rhoa, zeta_rhob, rho73, epscUEG_rho, epscUEG_zeta
            real(F64) :: rho, t2, t2_rho, t2_sigma, t2_zeta
            real(F64) :: H, H_rho, H_zeta, H_sigma, H_beta, expueg, expueg_epsc, expueg_phi
            real(F64) :: A, A_expueg, A_rho, A_zeta, A_beta, f, f_rho, f_zeta, f_sigma, f_beta
            real(F64) :: g1, g1_f, g2, g2_F, g2_t2, g2_beta, g2_rho, g2_zeta, g2_sigma
            real(F64) :: ec_rho, ec_zeta
            !
            ! Gamma constant defined just above Eq. 6
            !
            real(F64), parameter :: gamma = (ONE-log(TWO))/PI**2

            rho = rho_a + rho_b
            rho73 = rho**(SEVEN/THREE)
            zeta = (rho_a - rho_b) / rho
            zeta_rhoa = TWO * rho_b / rho**2
            zeta_rhob = -TWO * rho_a / rho**2
            !
            ! Dimensionless density gradient squared
            !            
            t2 = (PI/THREE)**(ONE/THREE)/16.0_F64 * sigma / (phi**2 * rho73)
            t2_sigma = (PI/THREE)**(ONE/THREE)/16.0_F64 / (phi**2 * rho73)
            t2_zeta = -TWO * t2 / phi * phi_zeta
            t2_rho = -SEVEN/THREE * t2 / rho
            
            expueg = exp(-epscUEG / (gamma * phi**3))
            expueg_epsc = -ONE / (gamma * phi**3) * expueg
            expueg_phi = THREE * epscUEG / (gamma * phi**4) * expueg
            !
            ! Function A defined in Eq. 8 of Ref. 1
            !            
            A = beta/gamma / (expueg - ONE)
            A_expueg = -beta/gamma / (expueg - ONE)**2
            !
            ! ecUEG = rho * epscUEG
            ! ecUEG_rho = epscUEG + rho * epscUEG_rho
            ! epscUEG_rho = (ecUEG_rho - epscUEG) / rho
            !
            epscUEG_rho = (ecUEG_rho - epscUEG) / rho
            epscUEG_zeta = ecUEG_zeta / rho
            A_rho = A_expueg * expueg_epsc * epscUEG_rho
            A_zeta = A_expueg * (expueg_phi * phi_zeta + expueg_epsc * epscUEG_zeta)
            A_beta = ONE/gamma / (expueg - ONE)

            f = A * t2
            f_rho = A_rho * t2 + A * t2_rho
            f_zeta = A_zeta * t2 + A * t2_zeta
            f_sigma = A * t2_sigma
            f_beta = A_beta * t2

            g1 = (ONE + f) / (ONE + f + f**2)
            g1_f = -f * (TWO + f) / (ONE + f + f**2)**2

            g2 = log(ONE + beta / gamma * t2 * g1)
            g2_f = beta * t2 / (gamma + beta * g1 * t2) * g1_f
            g2_t2 = beta * g1 / (gamma + beta * g1 * t2)
            g2_beta = g1 * t2 / (gamma + beta * g1 * t2) + g2_f * f_beta
            g2_rho = g2_f * f_rho + g2_t2 * t2_rho
            g2_zeta = g2_f * f_zeta + g2_t2 * t2_zeta
            g2_sigma = g2_f * f_sigma + g2_t2 * t2_sigma
            !
            ! H defined in Eq. 7 of Ref. 1
            !
            H = gamma * phi**3 * g2
            H_rho = gamma * phi**3 * g2_rho
            H_zeta = gamma * (THREE * phi**2 * phi_zeta * g2 + phi**3 * g2_zeta)
            H_sigma = gamma * phi**3 * g2_sigma
            H_beta  = gamma * phi**3 * g2_beta
            !
            ! Assemble UEG and GGA contributions into total correlation energy,
            ! Eq. 3 in Ref. 1
            !
            epsc = epscUEG + H
            ec_rho =  ecUEG_rho + H + rho * H_rho
            ec_zeta = ecUEG_zeta + rho * H_zeta
            ec_sigma = rho * H_sigma
            ec_beta = rho * H_beta
            ec_rhoa = ec_rho + ec_zeta * zeta_rhoa
            ec_rhob = ec_rho + ec_zeta * zeta_rhob
      end subroutine pbe_c_template_general


      subroutine pw92_c(epsc, ec_rho, ec_zeta, rho, rs, zeta)
            !
            ! Compute the PW92 correlation energy for arbitrary spin polarization.
            ! This subroutine employs extra digits of precision from K. Burke's 
            ! Fortran subroutine.
            !
            ! 1. Perdew, J.P., Wang, Y. Phys. Rev. B 45, 13244 (1992);
            !    doi: 10.1103/PhysRevB.45.13244
            !
            real(F64), intent(out) :: epsc
            real(F64), intent(out) :: ec_rho
            real(F64), intent(out) :: ec_zeta
            real(F64), intent(in)  :: rho
            real(F64), intent(in)  :: rs
            real(F64), intent(in)  :: zeta

            real(F64) :: epsc_rs, epsc_zeta

            call pw92_epsc(epsc, epsc_rs, epsc_zeta, rs, zeta)
            ec_rho = epsc + rho * epsc_rs * (-ONE/THREE) * rs / rho
            ec_zeta = rho * epsc_zeta
      end subroutine pw92_c

      
      subroutine pw92_epsc(epsc, epsc_rs, epsc_zeta, rs, zeta)
            !
            ! Compute the PW92 correlation energy per particle for arbitrary spin polarization.
            ! This subroutine employs extra digits of precision from K. Burke's 
            ! Fortran subroutine.
            !
            ! 1. Perdew, J.P., Wang, Y. Phys. Rev. B 45, 13244 (1992);
            !    doi: 10.1103/PhysRevB.45.13244
            !
            real(F64), intent(out) :: epsc
            real(F64), intent(out) :: epsc_rs
            real(F64), intent(out) :: epsc_zeta
            real(F64), intent(in)  :: rs
            real(F64), intent(in)  :: zeta

            real(F64) :: epsc1, epsc1_rs, epsc0, epsc0_rs
            real(F64) :: alphac, alphac_rs, zeta4
            real(F64) :: w, w_zeta, aa, bb
            real(F64) :: f, f_zeta, y1, y2
            real(F64), parameter :: fzz = FOUR/(NINE*(TWO**(ONE/THREE)-ONE))

            aa = ONE + zeta
            bb = ONE - zeta
            y1 = aa**(ONE/THREE)
            y2 = bb**(ONE/THREE)
            w = aa * y1 + bb * y2
            w_zeta = FOUR/THREE * (y1 - y2)
            !
            ! Spin-interpolation function defined in Eq. 9 of Ref. 1
            !
            f = (w - TWO) / (TWO**(FOUR/THREE)-TWO)
            f_zeta = w_zeta / (TWO**(FOUR/THREE)-TWO)
            !
            ! Eq. 7 in Ref. 1
            !
            call pw92_c_unpolar(epsc0, epsc0_rs, rs)
            call pw92_c_fullpolar(epsc1, epsc1_rs, rs)
            call pw92_alphac(alphac, alphac_rs, rs)
            zeta4 = zeta**4
            epsc = epsc0 + alphac * f / fzz * (ONE - zeta4) + (epsc1 - epsc0) * f * zeta4
            epsc_rs = epsc0_rs + alphac_rs * f / fzz * (ONE - zeta4) &
                  + (epsc1_rs - epsc0_rs) * F * zeta4
            epsc_zeta = f_zeta * (alphac / fzz * (ONE - zeta4) + (epsc1 - epsc0) * zeta4) &
                  + FOUR * zeta**3 * (-alphac * f / fzz + (epsc1 - epsc0) * f)
      end subroutine pw92_epsc
      

      subroutine pw92_c_unpolar(epsc, epsc_rs, rs)
            !
            ! Compute the PW92 correlation energy for spin-unpolarized electron gas.
            ! 1. Perdew, J.P., Wang, Y. Phys. Rev. B 45, 13244 (1992);
            !    doi: 10.1103/PhysRevB.45.13244
            !
            real(F64), intent(out) :: epsc
            real(F64), intent(out) :: epsc_rs
            real(F64), intent(in)  :: rs
            !
            ! Numerical constants of Table 1 in Ref. 1 for the epsc(rs, 0)
            ! column. Extra digits of precision are taken from K. Burke's
            ! Fortran subroutine.
            !
            real(F64), parameter :: A = 0.0310907_F64
            real(F64), parameter :: alpha1 = 0.21370_F64
            real(F64), parameter :: beta1 = 7.5957_F64
            real(F64), parameter :: beta2 = 3.5876_F64
            real(F64), parameter :: beta3 = 1.6382_F64
            real(F64), parameter :: beta4 = 0.49294_F64

            call G_Eq10(epsc, epsc_rs, rs, A, alpha1, beta1, beta2, beta3, beta4)
      end subroutine pw92_c_unpolar


      subroutine pw92_c_fullpolar(epsc, epsc_rs, rs)
            !
            ! Compute the PW92 correlation energy for fully spin-polarized electron gas.
            ! 1. Perdew, J.P., Wang, Y. Phys. Rev. B 45, 13244 (1992);
            !    doi: 10.1103/PhysRevB.45.13244
            !
            real(F64), intent(out) :: epsc
            real(F64), intent(out) :: epsc_rs
            real(F64), intent(in)  :: rs
            !
            ! Numerical constants of Table 1 in Ref. 1 for the epsc(rs, 1)
            ! column. Extra digits of precision are taken from K. Burke's
            ! Fortran subroutine.
            !
            real(F64), parameter :: A = 0.01554535_F64
            real(F64), parameter :: alpha1 = 0.20548_F64
            real(F64), parameter :: beta1 = 14.1189_F64
            real(F64), parameter :: beta2 = 6.1977_F64
            real(F64), parameter :: beta3 = 3.3662_F64
            real(F64), parameter :: beta4 = 0.62517_F64

            call G_Eq10(epsc, epsc_rs, rs, A, alpha1, beta1, beta2, beta3, beta4)
      end subroutine pw92_c_fullpolar


      subroutine pw92_alphac(alphac, alphac_rs, rs)
            !
            ! Compute alphac(rs) of Eq. 8.
            !
            ! 1. Perdew, J.P., Wang, Y. Phys. Rev. B 45, 13244 (1992);
            !    doi: 10.1103/PhysRevB.45.13244
            !
            real(F64), intent(out) :: alphac
            real(F64), intent(out) :: alphac_rs
            real(F64), intent(in)  :: rs
            !
            ! Numerical constants of Table 1 in Ref. 1 for the -alphac(rs)
            ! column. Extra digits of precision are taken from K. Burke's
            ! Fortran subroutine.
            !
            real(F64), parameter :: A = 0.0168869_F64
            real(F64), parameter :: alpha1 = 0.11125_F64
            real(F64), parameter :: beta1 = 10.357_F64
            real(F64), parameter :: beta2 = 3.6231_F64
            real(F64), parameter :: beta3 = 0.88026_F64
            real(F64), parameter :: beta4 = 0.49671_F64

            call G_Eq10(alphac, alphac_rs, rs, A, alpha1, beta1, beta2, beta3, beta4)
            alphac = -alphac
            alphac_rs = -alphac_rs
      end subroutine pw92_alphac


      subroutine G_Eq10(G, G_rs, rs, A, alpha1, beta1, beta2, beta3, beta4)
            !
            ! The interpolating function G defined in Eq. 10 of Ref. 1.
            ! P == 1 is assumed.
            !
            ! 1. Perdew, J.P., Wang, Y. Phys. Rev. B 45, 13244 (1992);
            !    doi: 10.1103/PhysRevB.45.13244
            !
            real(F64), intent(out) :: G
            real(F64), intent(out) :: G_rs
            real(F64), intent(in)  :: rs
            real(F64), intent(in)  :: A
            real(F64), intent(in)  :: alpha1
            real(F64), intent(in)  :: beta1
            real(F64), intent(in)  :: beta2
            real(F64), intent(in)  :: beta3
            real(F64), intent(in)  :: beta4

            real(F64) :: rs12, lny
            real(F64) :: x, y, x_rs, y_rs

            x = -TWO*A * (ONE + alpha1 * rs)
            x_rs = -TWO*A * alpha1
            rs12 = sqrt(rs)
            y = TWO*A * (beta1 * rs12 + beta2 * rs + beta3 * rs * rs12 + beta4 * rs**2)
            y_rs = TWO*A * (ONE/TWO*beta1 / rs12 + beta2 + THREE/TWO*beta3 * rs12 + TWO*beta4 * rs)
            lny = log(ONE + ONE / y)
            G = x * lny
            G_rs = x_rs * lny - x / (y + y**2) * y_rs
      end subroutine G_Eq10
end module pbe_c_energy
