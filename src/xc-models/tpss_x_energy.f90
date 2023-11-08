module tpss_x_energy
      use arithmetic
      use math_constants
      use modrzej2016_x_energy

      implicit none

      real(F64), parameter :: TPSS_X_RHO_THRESH = 1.0E-10_F64
      real(F64), parameter :: TPSS_X_TAU_THRESH = 1.0E-12_F64

contains

      subroutine ec_sr_tpss_x(sr_eps, sr_rho, sr_sig, sr_lap, sr_tau, rho, sigma, lapl, tau, omega)
            real(F64), intent(out) :: sr_eps
            real(F64), intent(out) :: sr_rho
            real(F64), intent(out) :: sr_sig
            real(F64), intent(out) :: sr_lap
            real(F64), intent(out) :: sr_tau
            real(F64), intent(in)  :: rho
            real(F64), intent(in)  :: sigma
            real(F64), intent(in)  :: lapl
            real(F64), intent(in)  :: tau
            real(F64), intent(in)  :: omega

            real(F64) :: fr_eps, fr_rho, fr_sig, fr_lap, fr_tau

            call tpss_x(fr_eps, fr_rho, fr_sig, fr_tau, rho, sigma, tau)
            fr_lap = ZERO
            call modrzej2016_sr_x_exact_curvature(sr_eps, sr_rho, sr_sig, sr_lap, sr_tau, rho, sigma, lapl, tau, &
                  fr_eps, fr_rho, fr_sig, fr_lap, fr_tau, omega)
      end subroutine ec_sr_tpss_x


      subroutine ec_sr_revtpss_x(sr_eps, sr_rho, sr_sig, sr_lap, sr_tau, rho, sigma, lapl, tau, omega)
            real(F64), intent(out) :: sr_eps
            real(F64), intent(out) :: sr_rho
            real(F64), intent(out) :: sr_sig
            real(F64), intent(out) :: sr_lap
            real(F64), intent(out) :: sr_tau
            real(F64), intent(in)  :: rho
            real(F64), intent(in)  :: sigma
            real(F64), intent(in)  :: lapl
            real(F64), intent(in)  :: tau
            real(F64), intent(in)  :: omega

            real(F64) :: fr_eps, fr_rho, fr_sig, fr_lap, fr_tau

            call revtpss_x(fr_eps, fr_rho, fr_sig, fr_tau, rho, sigma, tau)
            fr_lap = ZERO
            call modrzej2016_sr_x_exact_curvature(sr_eps, sr_rho, sr_sig, sr_lap, sr_tau, rho, sigma, lapl, tau, &
                  fr_eps, fr_rho, fr_sig, fr_lap, fr_tau, omega)
      end subroutine ec_sr_revtpss_x


      pure subroutine revtpss_x(eps, e_rho, e_sigma, e_tau, rho, sigma, tau)
            !
            ! Compute the revTPSS exchange energy
            !
            real(F64), intent(out) :: eps
            real(F64), intent(out) :: e_rho
            real(F64), intent(out) :: e_sigma
            real(F64), intent(out) :: e_tau
            real(F64), intent(in)  :: rho
            real(F64), intent(in)  :: sigma
            real(F64), intent(in)  :: tau
            
            real(F64) :: fx, e_fx, fx_rho, fx_sigma, fx_tau
            real(F64) epslda, epslda_rho
            !
            ! Compute the revTPSS exchange enhancement factor
            ! Note that the definition of tau differs by 1/2
            !
            call revtpss_enhancement_factor(fx, fx_rho, fx_sigma, fx_tau, rho, sigma, tau/TWO)            
            !
            ! LDA exchange energy density
            !
            epslda = (-THREE/FOUR)*(THREE/PI)**(ONE/THREE) * rho**(ONE/THREE)
            epslda_rho = ONE/THREE * epslda / rho

            eps = epslda * fx
            e_fx = rho * epslda
            e_rho = eps + rho * epslda_rho * fx + e_fx * fx_rho
            e_sigma = e_fx * fx_sigma
            !
            ! Switch back to the definition of tau without 1/2
            !
            e_tau = e_fx * fx_tau / TWO
      end subroutine revtpss_x


      pure subroutine tpss_x(eps, e_rho, e_sigma, e_tau, rho, sigma, tau)
            !
            ! Compute the TPSS exchange energy
            !
            real(F64), intent(out) :: eps
            real(F64), intent(out) :: e_rho
            real(F64), intent(out) :: e_sigma
            real(F64), intent(out) :: e_tau
            real(F64), intent(in)  :: rho
            real(F64), intent(in)  :: sigma
            real(F64), intent(in)  :: tau
            
            real(F64) :: fx, e_fx, fx_rho, fx_sigma, fx_tau
            real(F64) epslda, epslda_rho
            !
            ! Compute the TPSS exchange enhancement factor
            ! Note that the definition of tau differs by 1/2
            !
            call tpss_enhancement_factor(fx, fx_rho, fx_sigma, fx_tau, rho, sigma, tau/TWO)
            !
            ! LDA exchange energy density
            !
            epslda = (-THREE/FOUR)*(THREE/PI)**(ONE/THREE) * rho**(ONE/THREE)
            epslda_rho = ONE/THREE * epslda / rho

            eps = epslda * fx
            e_fx = rho * epslda
            e_rho = eps + rho * epslda_rho * fx + e_fx * fx_rho
            e_sigma = e_fx * fx_sigma
            !
            ! Switch back to the definition of tau without 1/2
            !
            e_tau = e_fx * fx_tau / TWO
      end subroutine tpss_x


      pure subroutine revtpss_enhancement_factor(fx, fx_rho, fx_sigma, fx_tau, rho, sigma, tau)
            ! ------------------------------------------------------------------------------------
            ! Compute the enhancement factor of the revTPSS exchange functional.[1]
            ! The implementation is according to the appendix in Ref. 2. The correctness and
            ! numerical stability of the code is verified using the Mathematica software.
            ! This subroutine accepts the input sigma=0 (exactly), but extremely small rho and tau
            ! should be screened out. Fx is related to the total exchange energy of a spin-
            ! compensated system as follows
            ! Ex = Int rho * Fx * epsLDA
            ! epsLDA = (-3/4) * (3/Pi)**(1/3) rho**(1/3)
            ! where rho is the total (alpha+beta) density. 
            ! ------------------------------------------------------------------------------------
            ! Fx 
            !           revTPSS Enhancement factor
            ! Fx_rho
            !           dFx/drho
            ! Fx_sigma
            !           dFx/dsigma
            ! Fx_tau
            !           dFx/dtau
            ! rho 
            !           Total electron density of a spin-compensated system (alpha+beta spin)
            ! sigma
            !           nabla rho * nabla rho (square of the gradient of rho)
            ! tau
            !           1/2 sum_i 2 * nabla Psi_i nabla Psi_i (kinetic energy density of 
            !           a spin-compensated system, including the 1/2 factor; Psi_i is a doubly
            !           occupied orbital)
            ! ------------------------------------------------------------------------------------
            ! 1. Perdew, J. P., Ruzsinszky, A., Csonka, G., Constantin, L.A., and Sun, J.
            !    Phys. Rev. Lett. 103, 026403 (2009); doi: 10.1103/PhysRevLett.103.026403
            ! 2. Sun, J., Marsman, M., Csonka, G.I., Ruzsinszky, A., Hao, P., Kim, Y.-S.,
            !    Kresse, G., and Perdew, J.P., Phys. Rev. B 85, 035117 (2011);
            !    doi: 10.1103/PhysRevB.84.035117
            !
            real(F64), intent(out) :: fx
            real(F64), intent(out) :: fx_rho
            real(F64), intent(out) :: fx_sigma
            real(F64), intent(out) :: fx_tau
            real(F64), intent(in)  :: rho
            real(F64), intent(in)  :: sigma
            real(F64), intent(in)  :: tau

            real(F64) :: p,  p_rho, p_sigma, p_denom, p2
            real(F64) :: tauw, tauw_rho, tauw_sigma
            real(F64) :: z, z_rho, z_sigma, z_tau
            real(F64) :: alpha, alpha_rho, alpha_tau, alpha_sigma
            real(F64) :: tau_ueg, tau_ueg_rho
            real(F64) :: rho83, rho53, rho103
            real(F64) :: t, t12, t32, qb, qb_alpha, qb_p
            real(F64) :: z2, s1, s1_p, s1_z
            real(F64) :: s2, s2_qb, s2_alpha, s2_p
            real(F64) :: tau2, w, w_rho, w_tau, t_qb, t_rho, t_sigma
            real(F64) :: s3, s3_rho, s3_sigma, s3_tau, s3_alpha, s3_p
            real(F64) :: s4, s4_p, s5, s5_z, s6, s6_p, s7, s7_p
            real(F64) :: t_p, t_alpha, t_z, x, x_p, x_alpha, x_z, x_rho, x_sigma, x_tau
            real(F64) :: fx_x, fx_alpha, fx_p, fx_z
            !
            ! Coefficient of the second-order gradient expansion 
            ! of the exchange energy
            !
            real(F64), parameter :: muge2 = 10.0_F64/81.0_F64
            real(F64), parameter :: b = 0.4_F64
            real(F64), parameter :: c = 2.35204_F64
            real(F64), parameter :: kappa = 0.804_F64
            real(F64), parameter :: e = 2.1677_F64
            real(F64), parameter :: e12 = sqrt(e)
            real(F64), parameter :: mu = 0.14_F64
            real(F64), parameter :: w_c1 = ONE/(96.0_F64*THREE**(ONE/THREE)*PI**(EIGHT/THREE))
            real(F64), parameter :: w_c2 = 9.0_F64/3200.0_F64

            rho53 = rho**(FIVE/THREE)
            rho103 = rho53**2
            rho83 = rho53 * rho

            tauw = sigma / (EIGHT * rho)
            tauw_sigma = ONE / (EIGHT * rho)
            tauw_rho = -tauw / rho

            z = tauw / tau
            z_rho = tauw_rho / tau
            z_sigma = tauw_sigma / tau
            z_tau = -z / tau

            p_denom = FOUR*(THREE*PI**2)**(TWO/THREE) * rho83
            p = sigma / p_denom
            p_rho = -EIGHT/THREE * p / rho
            p_sigma = ONE / p_denom

            tau_ueg = THREE/TEN * (THREE*PI**2)**(TWO/THREE) * rho53
            tau_ueg_rho = FIVE/THREE * tau_ueg / rho

            alpha = (tau - tauw) / tau_ueg
            alpha_rho = -tauw_rho / tau_ueg - alpha / tau_ueg * tau_ueg_rho
            alpha_sigma = -tauw_sigma / tau_ueg
            alpha_tau = ONE / tau_ueg

            t = ONE + b * alpha * (alpha - ONE)
            t12 = sqrt(t)
            t32 = t * t12
            qb = (NINE/TWENTY) * (alpha - ONE) / t12 + TWO/THREE * p
            qb_alpha = (NINE/FORTY) * (TWO + b * (alpha - ONE)) / t32
            qb_p = TWO/THREE
            
            z2 = z**2
            t = muge2 + c * z2*z / (ONE + z2)**2
            s1 = t * p
            s1_p = t
            s1_z = -c * z2 * (z2 - THREE) / (ONE + z2)**3 * p

            s2 = 146.0_F64/2025.0_F64 * qb**2
            s2_qb = 292.0_F64/2025.0_F64 * qb
            s2_alpha = s2_qb * qb_alpha
            s2_p = s2_qb * qb_p
            
            tau2 = tau**2
            w = sqrt(w_c1 / rho103 + w_c2 / tau2)
            w_rho = -FIVE/THREE * w_c1 / (rho83 * sqrt(w_c1 + w_c2 * rho103/tau2))
            w_tau = -w_c2 / (tau2 * sqrt(w_c1 * tau2 / rho103 + w_c2))
            t = -73.0_F64/405.0_F64 * qb * sigma / rho
            t_qb = -73.0_F64/405.0_F64 * sigma / rho
            t_rho = -t / rho
            t_sigma = -73.0_F64/405.0_F64 * qb / rho
            s3 = t * w
            s3_rho = t_rho * w + t * w_rho
            s3_sigma = t_sigma * w
            s3_tau = t * w_tau
            s3_alpha = t_qb * w * qb_alpha
            s3_p = t_qb * w * qb_p

            p2 = p**2
            s4 = ONE/kappa*(muge2)**2 * p2
            s4_p = TWO/kappa*(muge2)**2 * p

            s5 = 18.0_F64/25.0_F64 * e12 * muge2 * z2
            s5_z = 36.0_F64/25.0_F64 * e12 * muge2 * z

            s6 = e*mu * p2 * p
            s6_p = THREE*e*mu * p2

            s7 = (ONE + e12 * p)**2
            s7_p = TWO*e12 * (ONE + e12 * p)

            t = s1 + s2 + s3 + s4 + s5 + s6
            t_p = s1_p + s2_p + s3_p + s4_p + s6_p
            t_alpha = s2_alpha + s3_alpha
            t_z = s1_z + s5_z
            x = t / s7  
            x_p = t_p / s7 - x / s7 * s7_p
            x_alpha = t_alpha / s7
            x_z = t_z / s7
            x_rho = s3_rho / s7
            x_sigma = s3_sigma / s7
            x_tau = s3_tau / s7
            
            fx = ONE + kappa - kappa / (ONE + x / kappa)
            fx_x = ONE / (ONE + x / kappa)**2
            fx_p = fx_x * x_p
            fx_alpha = fx_x * x_alpha
            fx_z = fx_x * x_z
            fx_rho = fx_p * p_rho + fx_alpha * alpha_rho + fx_z * z_rho + fx_x * x_rho
            fx_sigma = fx_p * p_sigma + fx_alpha * alpha_sigma + fx_z * z_sigma + fx_x * x_sigma
            fx_tau = fx_alpha * alpha_tau + fx_z * z_tau + fx_x * x_tau
      end subroutine revtpss_enhancement_factor


      pure subroutine tpss_enhancement_factor(fx, fx_rho, fx_sigma, fx_tau, rho, sigma, tau)
            ! ---------------------------------------------------------------------------------------
            ! Compute the enhancement factor of the TPSS exchange functional.[1]
            ! The implementation is according to the appendinx in Ref. 2. The correctness and
            ! numerical stability of the code is verified using the Mathematica software.
            ! This subroutine accepts the input sigma=0 (exactly), but extremely small rho and tau
            ! should be screened out. Fx is related to the total exchange energy of a spin-
            ! compensated system as follows
            ! Ex = Int rho * Fx * epsLDA
            ! epsLDA = (-3/4) * (3/Pi)**(1/3) rho**(1/3)
            ! where rho is the total (alpha+beta) density.
            ! ---------------------------------------------------------------------------------------
            ! Fx 
            !           revTPSS Enhancement factor
            ! Fx_rho
            !           dFx/drho
            ! Fx_sigma
            !           dFx/dsigma
            ! Fx_tau
            !           dFx/dtau
            ! rho 
            !           Total electron density of a spin-compensated system (alpha+beta spin)
            ! sigma
            !           nabla rho * nabla rho (square of the gradient of rho)
            ! tau
            !           1/2 sum_i 2 * nabla Psi_i nabla Psi_i (kinetic energy density of 
            !           a spin-compensated system, including the 1/2 factor; Psi_i is a doubly
            !           occupied orbital)
            ! ------------------------------------------------------------------------------------
            ! 1. Perdew, J. P., Ruzsinszky, A., Csonka, G., Constantin, L.A., and Sun, J.
            !    Phys. Rev. Lett. 103, 026403 (2009); doi: 10.1103/PhysRevLett.103.026403
            ! 2. Sun, J., Marsman, M., Csonka, G.I., Ruzsinszky, A., Hao, P., Kim, Y.-S.,
            !    Kresse, G., and Perdew, J.P., Phys. Rev. B 85, 035117 (2011);
            !    doi: 10.1103/PhysRevB.84.035117
            !
            real(F64), intent(out) :: fx
            real(F64), intent(out) :: fx_rho
            real(F64), intent(out) :: fx_sigma
            real(F64), intent(out) :: fx_tau
            real(F64), intent(in)  :: rho
            real(F64), intent(in)  :: sigma
            real(F64), intent(in)  :: tau

            real(F64) :: p,  p_rho, p_sigma, p_denom, p2
            real(F64) :: tauw, tauw_rho, tauw_sigma
            real(F64) :: z, z_rho, z_sigma, z_tau
            real(F64) :: alpha, alpha_rho, alpha_tau, alpha_sigma
            real(F64) :: tau_ueg, tau_ueg_rho
            real(F64) :: rho83, rho53, rho103
            real(F64) :: t, t12, t32, qb, qb_alpha, qb_p
            real(F64) :: z2, s1, s1_p, s1_z
            real(F64) :: s2, s2_qb, s2_alpha, s2_p
            real(F64) :: tau2, w, w_rho, w_tau, t_qb, t_rho, t_sigma
            real(F64) :: s3, s3_rho, s3_sigma, s3_tau, s3_alpha, s3_p
            real(F64) :: s4, s4_p, s5, s5_z, s6, s6_p, s7, s7_p
            real(F64) :: t_p, t_alpha, t_z, x, x_p, x_alpha, x_z, x_rho, x_sigma, x_tau
            real(F64) :: fx_x, fx_alpha, fx_p, fx_z
            !
            ! Coefficient of the second-order gradient expansion 
            ! of the exchange energy
            !
            real(F64), parameter :: muge2 = 10.0_F64/81.0_F64
            real(F64), parameter :: b = 0.4_F64
            real(F64), parameter :: c = 1.59096_F64
            real(F64), parameter :: kappa = 0.804_F64
            real(F64), parameter :: e = 1.537_F64
            real(F64), parameter :: e12 = sqrt(e)
            real(F64), parameter :: mu = 0.21951_F64
            real(F64), parameter :: w_c1 = ONE/(96.0_F64*THREE**(ONE/THREE)*PI**(EIGHT/THREE))
            real(F64), parameter :: w_c2 = 9.0_F64/3200.0_F64

            rho53 = rho**(FIVE/THREE)
            rho103 = rho53**2
            rho83 = rho53 * rho

            tauw = sigma / (EIGHT * rho)
            tauw_sigma = ONE / (EIGHT * rho)
            tauw_rho = -tauw / rho

            z = tauw / tau
            z_rho = tauw_rho / tau
            z_sigma = tauw_sigma / tau
            z_tau = -z / tau

            p_denom = FOUR*(THREE*PI**2)**(TWO/THREE) * rho83
            p = sigma / p_denom
            p_rho = -EIGHT/THREE * p / rho
            p_sigma = ONE / p_denom

            tau_ueg = THREE/TEN * (THREE*PI**2)**(TWO/THREE) * rho53
            tau_ueg_rho = FIVE/THREE * tau_ueg / rho

            alpha = (tau - tauw) / tau_ueg
            alpha_rho = -tauw_rho / tau_ueg - alpha / tau_ueg * tau_ueg_rho
            alpha_sigma = -tauw_sigma / tau_ueg
            alpha_tau = ONE / tau_ueg

            t = ONE + b * alpha * (alpha - ONE)
            t12 = sqrt(t)
            t32 = t * t12
            qb = (NINE/TWENTY) * (alpha - ONE) / t12 + TWO/THREE * p
            qb_alpha = (NINE/FORTY) * (TWO + b * (alpha - ONE)) / t32
            qb_p = TWO/THREE
            
            z2 = z**2
            t = muge2 + c * z2 / (ONE + z2)**2
            s1 = t * p
            s1_p = t
            s1_z = -c * TWO * z * (z2 - ONE) / (ONE + z2)**3 * p

            s2 = 146.0_F64/2025.0_F64 * qb**2
            s2_qb = 292.0_F64/2025.0_F64 * qb
            s2_alpha = s2_qb * qb_alpha
            s2_p = s2_qb * qb_p
            
            tau2 = tau**2
            w = sqrt(w_c1 / rho103 + w_c2 / tau2)
            w_rho = -FIVE/THREE * w_c1 / (rho83 * sqrt(w_c1 + w_c2 * rho103/tau2))
            w_tau = -w_c2 / (tau2 * sqrt(w_c1 * tau2 / rho103 + w_c2))
            t = -73.0_F64/405.0_F64 * qb * sigma / rho
            t_qb = -73.0_F64/405.0_F64 * sigma / rho
            t_rho = -t / rho
            t_sigma = -73.0_F64/405.0_F64 * qb / rho
            s3 = t * w
            s3_rho = t_rho * w + t * w_rho
            s3_sigma = t_sigma * w
            s3_tau = t * w_tau
            s3_alpha = t_qb * w * qb_alpha
            s3_p = t_qb * w * qb_p

            p2 = p**2
            s4 = ONE/kappa*(muge2)**2 * p2
            s4_p = TWO/kappa*(muge2)**2 * p

            s5 = 18.0_F64/25.0_F64 * e12 * muge2 * z2
            s5_z = 36.0_F64/25.0_F64 * e12 * muge2 * z

            s6 = e*mu * p2 * p
            s6_p = THREE*e*mu * p2

            s7 = (ONE + e12 * p)**2
            s7_p = TWO*e12 * (ONE + e12 * p)

            t = s1 + s2 + s3 + s4 + s5 + s6
            t_p = s1_p + s2_p + s3_p + s4_p + s6_p
            t_alpha = s2_alpha + s3_alpha
            t_z = s1_z + s5_z
            x = t / s7  
            x_p = t_p / s7 - x / s7 * s7_p
            x_alpha = t_alpha / s7
            x_z = t_z / s7
            x_rho = s3_rho / s7
            x_sigma = s3_sigma / s7
            x_tau = s3_tau / s7
            
            fx = ONE + kappa - kappa / (ONE + x / kappa)
            fx_x = ONE / (ONE + x / kappa)**2
            fx_p = fx_x * x_p
            fx_alpha = fx_x * x_alpha
            fx_z = fx_x * x_z
            fx_rho = fx_p * p_rho + fx_alpha * alpha_rho + fx_z * z_rho + fx_x * x_rho
            fx_sigma = fx_p * p_sigma + fx_alpha * alpha_sigma + fx_z * z_sigma + fx_x * x_sigma
            fx_tau = fx_alpha * alpha_tau + fx_z * z_tau + fx_x * x_tau
      end subroutine tpss_enhancement_factor
end module tpss_x_energy
