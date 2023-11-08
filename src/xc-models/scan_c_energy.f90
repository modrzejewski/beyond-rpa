module scan_c_energy
      use arithmetic
      use math_constants
      use scan_x_energy
      use pbe_c_energy

      implicit none
      !
      ! Threshold for total density (alpha + beta spin).
      ! No energy and derivatives are computed if rho<SCAN_C_RHO_THRESH
      !
      real(F64), parameter :: SCAN_C_RHO_THRESH = 1.0E-10_F64
      !
      ! If one of spin-densities is below SCAN_C_FULLPOLAR_THRESH,
      ! the system is deemed fully spin-polarized
      !
      real(F64), parameter :: SCAN_C_FULLPOLAR_THRESH = 1.0E-13_F64

contains

      subroutine epsc0S10(epsc0, epsc0_rs, epsc0_zeta, epsc0_s2, rs, zeta, s2)
            real(F64), intent(out) :: epsc0
            real(F64), intent(out) :: epsc0_rs
            real(F64), intent(out) :: epsc0_zeta
            real(F64), intent(out) :: epsc0_s2
            real(F64), intent(in)  :: rs
            real(F64), intent(in)  :: zeta
            real(F64), intent(in)  :: s2

            real(F64) :: aa, bb, y1, y2, dx, dx_zeta
            real(F64) :: gc1, gc1_zeta, gc2, gc2_zeta, gc, gc_zeta
            real(F64) :: epscLDA0, epscLDA0_rs, rs12
            real(F64) :: gInf0, gInf0_s2, w0, w0_rs
            real(F64) :: H0, H0_rs, H0_s2
            real(F64), parameter :: gc_coeff = 2.3631_F64
            real(F64), parameter :: b1c = 0.0285764_F64
            real(F64), parameter :: b2c = 0.0889_F64
            real(F64), parameter :: b3c = 0.125541_F64
            real(F64), parameter :: ChiInf0 = 0.128025852626258169_F64
            !
            ! Equation S12
            !
            aa = (ONE + zeta)
            bb = (ONE - zeta)
            y1 = aa**(ONE/THREE)
            y2 = bb**(ONE/THREE)
            dx = (ONE/TWO) * (aa * y1 + bb * y2)
            dx_zeta = (TWO/THREE) * (y1 - y2)
            !
            ! Equation S11
            !
            gc1 = ONE+gc_coeff - gc_coeff * dx
            gc1_zeta = -gc_coeff * dx_zeta
            gc2 = ONE - zeta**12
            gc2_zeta = -TWELVE * zeta**11
            gc = gc1 * gc2
            gc_zeta = gc1_zeta * gc2 + gc1 * gc2_zeta
            !
            ! Equation S13
            !
            rs12 = sqrt(rs)
            epscLDA0 = -b1c / (ONE + b2c * rs12 + b3c * rs)
            epscLDA0_rs = (b1c*b3c + (b1c*b2c/TWO) / rs12) / (ONE + b2c * rs12 + b3c * rs)**2
            !
            ! Equation S16
            !
            gInf0 = ONE / (ONE + FOUR*ChiInf0 * s2)**(ONE/FOUR)
            gInf0_s2 = -ChiInf0 * gInf0 / (ONE + FOUR*ChiInf0 * s2)
            !
            ! Equation S15
            !
            w0 = exp(-epscLDA0 / b1c) - ONE
            w0_rs = -epscLDA0_rs / b1c * exp(-epscLDA0 / b1c)
            !
            ! Equation S14
            !
            H0 = b1c * log(ONE + w0 * (ONE - gInf0))
            H0_rs = b1c * (gInf0 - ONE) / (w0 * (gInf0 - ONE) - ONE) * w0_rs
            H0_s2 = b1c * w0 / (w0 * (gInf0 - ONE) - ONE) * gInf0_s2
            !
            ! Equation S10
            !
            epsc0 = (epscLDA0 + H0) * gc
            epsc0_rs = (epscLDA0_rs + H0_rs) * gc
            epsc0_zeta = (epscLDA0 + H0) * gc_zeta
            epsc0_s2 = H0_s2 * gc
      end subroutine epsc0S10


      subroutine beta_scan(beta, beta_rs, rs)
            real(F64), intent(out) :: beta
            real(F64), intent(out) :: beta_rs
            real(F64), intent(in)  :: rs

            real(F64), parameter :: a0 = 0.066725_F64
            real(F64), parameter :: a1 = 0.0066725_F64
            real(F64), parameter :: b0 = 1.000000_F64
            real(F64), parameter :: b1 = 0.1778_F64
            real(F64), parameter :: c0 = -0.005191205_F64

            beta = (a0 + a1 * rs) / (b0 + b1 * rs)
            beta_rs = c0 / (b0 + b1 * rs)**2
      end subroutine beta_scan
      
      
      subroutine epsc1S5(epsc1, epsc1_rs, epsc1_zeta, epsc1_s2, rs, s2, &
            phi, phi_zeta, epscUEG, epscUEG_rs, epscUEG_zeta)            
            real(F64), intent(out) :: epsc1
            real(F64), intent(out) :: epsc1_rs
            real(F64), intent(out) :: epsc1_zeta
            real(F64), intent(out) :: epsc1_s2
            real(F64), intent(in)  :: rs
            real(F64), intent(in)  :: s2
            real(F64), intent(in)  :: phi
            real(F64), intent(in)  :: phi_zeta
            real(F64), intent(in)  :: epscUEG
            real(F64), intent(in)  :: epscUEG_rs
            real(F64), intent(in)  :: epscUEG_zeta

            real(F64) :: w1, w1_rs, w1_zeta, e1, x, x_rs, x_zeta
            real(F64) :: A, A_w1, A_rs, A_zeta, t2, t2_rs, t2_s2, t2_zeta
            real(F64) :: y, y_s2, y_rs, y_zeta, g, g_y, g_rs, g_zeta, g_s2
            real(F64) :: z, z_s2, z_rs, z_zeta
            real(F64) :: beta, beta_rs
            real(F64) :: H1, H1_z, H1_rs, H1_zeta, H1_s2, lnz, lnz_z
            real(F64), parameter :: gamma = 0.031091_F64
            !
            ! Equation S7
            !
            x = -epscUEG / (gamma * phi**3)
            x_rs = -epscUEG_rs / (gamma * phi**3)
            x_zeta = (-epscUEG_zeta + THREE * epscUEG / phi * phi_zeta) / (gamma * phi**3)
            e1 = exp(x)
            w1 = e1 - ONE
            w1_rs = e1 * x_rs
            w1_zeta = e1 * x_zeta
            !
            ! Equation S8
            !
            call beta_scan(beta, beta_rs, rs)
            A = beta / (gamma * w1)
            A_w1 = -beta / (gamma * w1**2)
            A_rs = beta_rs / (gamma * w1) + A_w1 * w1_rs
            A_zeta = A_w1 * w1_zeta
            !
            ! Equation S9
            !
            t2 = (THREE*PI**2/SIXTEEN)**(TWO/THREE) * s2 / (phi**2 * rs)
            t2_s2 = (THREE*PI**2/SIXTEEN)**(TWO/THREE) / (phi**2 * rs)
            t2_rs = -(THREE*PI**2/SIXTEEN)**(TWO/THREE) * s2 / (phi**2 * rs**2)
            t2_zeta = -TWO * (THREE*PI**2/SIXTEEN)**(TWO/THREE) * s2 / (phi**3 * rs) * phi_zeta
            y = A * t2
            y_s2 = A * t2_s2
            y_rs = A_rs * t2 + A * t2_rs
            y_zeta =  A_zeta * t2 + A * t2_zeta
            g = ONE / (ONE + FOUR * y)**(ONE/FOUR)
            g_y = -g / (ONE + FOUR * y)
            g_rs = g_y * y_rs
            g_zeta = g_y * y_zeta
            g_s2 = g_y * y_s2
            !
            ! Equation S6
            !
            z = w1 * (ONE - g)
            z_rs = w1_rs * (ONE - g) - w1 * g_rs
            z_zeta = w1_zeta * (ONE - g) - w1 * g_zeta
            z_s2 = -w1 * g_s2
            lnz = log(ONE + z)
            lnz_z = ONE / (ONE + z)
            H1 = gamma * phi**3 * lnz
            H1_z = gamma * phi**3 * lnz_z
            H1_rs = H1_z * z_rs
            H1_zeta = THREE * gamma * phi**2 * lnz * phi_zeta + H1_z * z_zeta
            H1_s2 = H1_z * z_s2
            !
            ! Equation S5
            !
            epsc1 = epscUEG + H1
            epsc1_rs = epscUEG_rs + H1_rs
            epsc1_zeta = epscUEG_zeta + H1_zeta
            epsc1_s2 = H1_s2
      end subroutine epsc1S5


      subroutine scan_c_ingredients(s2, s2_rho, s2_sigma, alpha, alpha_rho, alpha_zeta, &
            alpha_sigma, alpha_tau, rho, zeta, sigma, tau)
            !
            ! Compute S**2 and alpha ingredients of the SCAN correlation energy.
            ! Note that, according to the definition given in the supporting info for Ref. 1,
            ! alpha includes the spin-scaling factor ds.
            !
            ! 1. Sun, J., Ruzsinszky, A., Perdew, J.P., Strongly Constrained
            !    and Appropriately Normed Semilocal Density Functional,
            !    Phys. Rev. Lett. 115, 036402 (2015); doi: 10.1103/PhysRevLett.115.036402
            !
            real(F64), intent(out) :: s2
            real(F64), intent(out) :: s2_rho
            real(F64), intent(out) :: s2_sigma
            real(F64), intent(out) :: alpha
            real(F64), intent(out) :: alpha_rho
            real(F64), intent(out) :: alpha_zeta
            real(F64), intent(out) :: alpha_sigma
            real(F64), intent(out) :: alpha_tau
            real(F64), intent(in)  :: rho
            real(F64), intent(in)  :: zeta
            real(F64), intent(in)  :: sigma
            real(F64), intent(in)  :: tau

            real(F64), parameter :: cTauUEG = (THREE/TEN)*(THREE*PI**2)**(TWO/THREE)
            real(F64), parameter :: cS = (TWO * (THREE * PI**2)**(ONE/THREE))**2
            real(F64) :: tauw, tauw_sigma, tauw_rho
            real(F64) :: tauUEG, tauUEG_rho, tauUEG_zeta, ds, ds_zeta
            real(F64) :: aa, bb, y1, y2
            !
            ! Weizsacker kinetic energy (alpha+beta spin, including the 1/2 factor)
            !
            tauw = sigma / (EIGHT * rho)
            tauw_sigma = ONE / (EIGHT * rho)
            tauw_rho = -sigma / (EIGHT * rho**2)
            !
            ! UEG kinetic energy (including the 1/2 factor)
            !
            aa = ONE + zeta
            bb = ONE - zeta
            y1 = aa**(TWO/THREE)
            y2 = bb**(TWO/THREE)
            ds = (ONE/TWO) * (aa * y1 + bb * y2)
            ds_zeta = (FIVE/SIX) * (y1 - y2)
            tauUEG = cTauUEG * rho**(FIVE/THREE) * ds
            !
            ! Note the ds factor defined in the supporting info for Ref. 1
            !
            tauUEG_rho = (FIVE/THREE)*cTauUEG * rho**(TWO/THREE) * ds
            tauUEG_zeta = cTauUEG * rho**(FIVE/THREE) * ds_zeta
            alpha = (ONE/TWO * tau - tauw) / tauUEG
            alpha_tau = ONE/TWO / tauUEG
            alpha_sigma = -tauw_sigma / tauUEG
            alpha_rho = -tauw_rho / tauUEG - alpha / tauUEG * tauUEG_rho
            alpha_zeta = -alpha / tauUEG * tauUEG_zeta
            !
            ! Reduced gradient
            !
            s2 = sigma / (cS * rho**(EIGHT/THREE))
            s2_sigma = ONE / (cS * rho**(EIGHT/THREE))
            s2_rho = -(EIGHT/THREE) * s2 / rho
      end subroutine scan_c_ingredients
      

      subroutine scan_c_template(eps, eps_rs, eps_zeta, eps_s2, eps_alpha, rs, zeta, s2, alpha, &
            epscUEG, epscUEG_rs, epscUEG_zeta, phi, phi_zeta)
            real(F64), intent(out) :: eps
            real(F64), intent(out) :: eps_rs
            real(F64), intent(out) :: eps_zeta
            real(F64), intent(out) :: eps_s2
            real(F64), intent(out) :: eps_alpha
            real(F64), intent(in)  :: rs
            real(F64), intent(in)  :: zeta
            real(F64), intent(in)  :: s2
            real(F64), intent(in)  :: alpha
            real(F64), intent(in)  :: epscUEG
            real(F64), intent(in)  :: epscUEG_rs
            real(F64), intent(in)  :: epscUEG_zeta
            real(F64), intent(in)  :: phi
            real(F64), intent(in)  :: phi_zeta

            real(F64) :: fc, fc_alpha
            real(F64) :: epsc1, epsc1_rs, epsc1_zeta, epsc1_s2
            real(F64) :: epsc0, epsc0_rs, epsc0_zeta, epsc0_s2
            real(F64), parameter :: c1c = 0.64_F64
            real(F64), parameter :: c2c = 1.5_F64
            real(F64), parameter :: dc = 0.7_F64
            
            call scan_interp_func(fc, fc_alpha, alpha, c1c, c2c, dc)
            call epsc0S10(epsc0, epsc0_rs, epsc0_zeta, epsc0_s2, rs, zeta, s2)
            call epsc1S5(epsc1, epsc1_rs, epsc1_zeta, epsc1_s2, rs, s2, &
                  phi, phi_zeta, epscUEG, epscUEG_rs, epscUEG_zeta)
            !
            ! Equation S3
            !
            eps = epsc1 + fc * (epsc0 - epsc1)
            eps_rs = epsc1_rs + fc * (epsc0_rs - epsc1_rs)
            eps_zeta = epsc1_zeta  + fc * (epsc0_zeta - epsc1_zeta)
            eps_s2 = epsc1_s2 + fc * (epsc0_s2 - epsc1_s2)
            eps_alpha = fc_alpha * (epsc0 - epsc1)
      end subroutine scan_c_template


      subroutine u_scan_c(epsc, ec_rhoa, ec_rhob, ec_sigaa, ec_sigab, ec_sigbb, ec_taua, ec_taub, &
            rho_a, rho_b, sigma_aa, sigma_ab, sigma_bb, tau_a, tau_b)
            !
            ! SCAN correlation energy of a spin-polarized system. [1] The kinetic energy densities
            ! (tau_a and tau_b) are defined without the 1/2 factor. The derivatives corresponding
            ! to vanishing densities are set to zero.
            !
            ! 1. Sun, J., Ruzsinszky, A., Perdew, J.P., Strongly Constrained
            !    and Appropriately Normed Semilocal Density Functional,
            !    Phys. Rev. Lett. 115, 036402 (2015); doi: 10.1103/PhysRevLett.115.036402
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

            real(F64) :: rho, rs, rs_rho, zeta, zeta_rhoa, zeta_rhob, sigma, tau
            real(F64) :: epscUEG, epscUEG_rs, epscUEG_zeta
            real(F64) :: y1, y2, phi, phi_zeta
            real(F64) :: epsc_rhoa, epsc_rhob, epsc_s2, epsc_alpha, epsc_rs, epsc_zeta
            real(F64) :: s2, s2_rho, s2_sigma, alpha, alpha_rho, alpha_zeta, alpha_sigma, alpha_tau

            if ((rho_a+rho_b) > SCAN_C_RHO_THRESH .and. rho_a > SCAN_C_FULLPOLAR_THRESH &
                  .and. rho_b > SCAN_C_FULLPOLAR_THRESH) then        
                  rho = rho_a + rho_b
                  sigma = sigma_aa + TWO * sigma_ab + sigma_bb
                  tau = tau_a + tau_b
                  rs = (THREE/(FOUR*PI))**(ONE/THREE) / rho**(ONE/THREE)
                  rs_rho = -(ONE/THREE) * rs / rho
                  zeta = (rho_a - rho_b) / rho
                  zeta_rhoa = TWO * rho_b / rho**2
                  zeta_rhob = -TWO * rho_a / rho**2
                  call pw92_epsc(epscUEG, epscUEG_rs, epscUEG_zeta, rs, zeta)
                  y1 = (ONE + zeta)**(ONE/THREE)
                  y2 = (ONE - zeta)**(ONE/THREE)
                  phi = (y1**2 + y2**2) / TWO
                  phi_zeta = (ONE / y1 - ONE / y2) / THREE
                  call scan_c_ingredients(s2, s2_rho, s2_sigma, alpha, alpha_rho, alpha_zeta, &
                        alpha_sigma, alpha_tau, rho, zeta, sigma, tau)
                  call scan_c_template(epsc, epsc_rs, epsc_zeta, epsc_s2, epsc_alpha, rs, zeta, s2, alpha, &
                        epscUEG, epscUEG_rs, epscUEG_zeta, phi, phi_zeta)
                  epsc_rhoa = epsc_rs * rs_rho + epsc_zeta * zeta_rhoa + epsc_s2 * s2_rho &
                        + epsc_alpha * (alpha_rho + alpha_zeta * zeta_rhoa)
                  epsc_rhob = epsc_rs * rs_rho + epsc_zeta * zeta_rhob + epsc_s2 * s2_rho &
                        + epsc_alpha * (alpha_rho + alpha_zeta * zeta_rhob)
                  ec_rhoa = epsc + rho * epsc_rhoa
                  ec_rhob = epsc + rho * epsc_rhob
                  ec_sigaa = rho * (epsc_s2 * s2_sigma + epsc_alpha * alpha_sigma)
                  ec_sigab = ec_sigaa * TWO
                  ec_sigbb = ec_sigaa
                  ec_taua = rho * epsc_alpha * alpha_tau
                  ec_taub = ec_taua
            else if ((rho_a+rho_b) > SCAN_C_RHO_THRESH) then
                  !
                  ! One of spin-densities is smaller than SCAN_C_FULLPOLAR_THRESH.
                  !
                  ! Special case of fully-polarized densities is needed to avoid
                  ! division by zero in dphi/dzeta. All derivatives corresponding
                  ! to the vanishing density are set to zero.
                  !
                  if (rho_a > SCAN_C_FULLPOLAR_THRESH) then
                        zeta = ONE
                  else
                        zeta = -ONE
                  end if
                  rho = rho_a + rho_b
                  sigma = sigma_aa + TWO * sigma_ab + sigma_bb
                  tau = tau_a + tau_b
                  rs = (THREE/(FOUR*PI))**(ONE/THREE) / rho**(ONE/THREE)
                  rs_rho = (-ONE/THREE) * rs / rho
                  phi = ONE / TWO**(ONE/THREE)
                  phi_zeta = ZERO
                  call pw92_c_fullpolar(epscUEG, epscUEG_rs, rs)
                  epscUEG_zeta = ZERO
                  call scan_c_ingredients(s2, s2_rho, s2_sigma, alpha, alpha_rho, alpha_zeta, &
                        alpha_sigma, alpha_tau, rho, zeta, sigma, tau)
                  call scan_c_template(epsc, epsc_rs, epsc_zeta, epsc_s2, epsc_alpha, rs, zeta, s2, alpha, &
                        epscUEG, epscUEG_rs, epscUEG_zeta, phi, phi_zeta)
                  if (rho_a > SCAN_C_FULLPOLAR_THRESH) then
                        epsc_rhoa = epsc_rs * rs_rho + epsc_s2 * s2_rho + epsc_alpha * alpha_rho
                        ec_rhoa = epsc + rho * epsc_rhoa
                        ec_rhob = ZERO
                        ec_sigaa = rho * (epsc_s2 * s2_sigma + epsc_alpha * alpha_sigma)
                        ec_sigbb = ZERO
                        ec_taua = rho * epsc_alpha * alpha_tau
                        ec_taub = ZERO
                  else
                        ec_rhoa = ZERO
                        epsc_rhob = epsc_rs * rs_rho + epsc_s2 * s2_rho + epsc_alpha * alpha_rho
                        ec_rhob = epsc + rho * epsc_rhob
                        ec_sigaa = ZERO
                        ec_sigbb = rho * (epsc_s2 * s2_sigma + epsc_alpha * alpha_sigma)
                        ec_taua = ZERO
                        ec_taub = rho * epsc_alpha * alpha_tau
                  end if
                  ec_sigab = ZERO
            else
                  epsc = ZERO
                  ec_rhoa = ZERO
                  ec_rhob = ZERO
                  ec_sigaa = ZERO
                  ec_sigab = ZERO
                  ec_sigbb = ZERO
                  ec_taua = ZERO
                  ec_taub = ZERO
            end if
      end subroutine u_scan_c


      subroutine scan_c(epsc, ec_rho, ec_sigma, ec_tau, rho, sigma, tau)
            !
            ! SCAN correlation energy of a spin-unpolarized system. [1] The kinetic energy density
            ! (tau) is defined without the 1/2 factor.
            !
            ! 1. Sun, J., Ruzsinszky, A., Perdew, J.P., Strongly Constrained
            !    and Appropriately Normed Semilocal Density Functional,
            !    Phys. Rev. Lett. 115, 036402 (2015); doi: 10.1103/PhysRevLett.115.036402
            !
            real(F64), intent(out) :: epsc
            real(F64), intent(out) :: ec_rho
            real(F64), intent(out) :: ec_sigma
            real(F64), intent(out) :: ec_tau
            real(F64), intent(in)  :: rho
            real(F64), intent(in)  :: sigma
            real(F64), intent(in)  :: tau

            real(F64) :: rs, rs_rho, zeta
            real(F64) :: epscUEG, epscUEG_rs, epscUEG_zeta
            real(F64) :: phi, phi_zeta
            real(F64) :: epsc_rho, epsc_s2, epsc_alpha, epsc_rs, epsc_zeta
            real(F64) :: s2, s2_rho, s2_sigma, alpha, alpha_rho, alpha_zeta, alpha_sigma, alpha_tau

            if (rho > SCAN_C_RHO_THRESH) then        
                  rs = (THREE/(FOUR*PI))**(ONE/THREE) / rho**(ONE/THREE)
                  rs_rho = -(ONE/THREE) * rs / rho
                  zeta = ZERO
                  call pw92_c_unpolar(epscUEG, epscUEG_rs, rs)
                  phi = ONE
                  phi_zeta = ZERO
                  call scan_c_ingredients(s2, s2_rho, s2_sigma, alpha, alpha_rho, alpha_zeta, &
                        alpha_sigma, alpha_tau, rho, zeta, sigma, tau)
                  call scan_c_template(epsc, epsc_rs, epsc_zeta, epsc_s2, epsc_alpha, rs, zeta, s2, alpha, &
                        epscUEG, epscUEG_rs, epscUEG_zeta, phi, phi_zeta)
                  epsc_rho = epsc_rs * rs_rho + epsc_s2 * s2_rho + epsc_alpha * alpha_rho
                  ec_rho = epsc + rho * epsc_rho
                  ec_sigma = rho * (epsc_s2 * s2_sigma + epsc_alpha * alpha_sigma)
                  ec_tau = rho * epsc_alpha * alpha_tau
            else
                  epsc = ZERO
                  ec_rho = ZERO
                  ec_sigma = ZERO
                  ec_tau = ZERO
            end if
      end subroutine scan_c
end module scan_c_energy
