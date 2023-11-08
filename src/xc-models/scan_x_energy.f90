module scan_x_energy
      use arithmetic
      use math_constants
      use modrzej2016_x_energy
      
      implicit none

      real(F64), parameter :: SCAN_X_RHO_THRESH = 1.0E-10_F64
      
contains

      subroutine ec_sr_scan_x(sr_eps, sr_rho, sr_sig, sr_lap, sr_tau, rho, sigma, lapl, tau, omega)
            ! --------------------------------------------------------------------------------------
            ! Compute the short-range SCAN exchange energy using the meta-GGA Becke-Roussel-type
            ! exchange hole. This subroutine is for spin-compensated densities. Use the spin-scaling
            ! relation Ex[RhoA, RhoB] = 1/2(Ex[2*RhoA] + Ex[2*RhoB]) to compute the exchange energy
            ! for open-shell densities.
            ! --------------------------------------------------------------------------------------
            ! SR_EPS
            !           Short-range exchange energy density. Short-range exchange energy:
            !           ExSR = rho * sr_eps
            ! SR_RHO, SR_SIG, SR_LAP, SR_TAU
            !           Derivatives of ExSR: dExSR/dRho, dExSR/dSigma, dExSR/dLapl, dExSR/dTau
            ! RHO
            !           Total electron density (alpha+beta spin contributions)
            !           of a closed-shell system          
            ! SIGMA
            !           Density gradient = (Nabla RHO) * (Nabla RHO)
            ! LAPL
            !           Density Laplacian = Nabla^2 RHO
            ! TAU
            !           Kinetic energy density = 2 * SUM_i (Nabla Phi_i) * (Nabla Phi_i), where 
            !           the summation is over occupied orbitals. Note that the definition of TAU
            !           does not include the 1/2 factor which some authors use.
            ! OMEGA
            !           Range-separation parameter of the long-range potential erf(omega * r) / r
            !
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

            call scan_x(fr_eps, fr_rho, fr_sig, fr_tau, rho, sigma, tau)
            fr_lap = ZERO
            call modrzej2016_sr_x_exact_curvature(sr_eps, sr_rho, sr_sig, sr_lap, sr_tau, rho, sigma, lapl, tau, &
                  fr_eps, fr_rho, fr_sig, fr_lap, fr_tau, omega)
      end subroutine ec_sr_scan_x


      subroutine ec_sh_scan_x(eps, ex_rho, ex_sig, ex_lap, ex_tau, rho, sigma, lapl, tau, srfrac, omega)
            ! --------------------------------------------------------------------------------------
            ! Compute the semilocal part of the screened-hybrid SCAN exchange energy using
            ! the meta-GGA Becke-Roussel-type exchange hole. This subroutine is for
            ! spin-compensated densities. Use the spin-scaling relation
            ! Ex[RhoA, RhoB] = 1/2(Ex[2*RhoA] + Ex[2*RhoB]) to compute the exchange energy
            ! for open-shell densities.
            ! --------------------------------------------------------------------------------------
            ! EPS
            !           Short-range exchange energy density. Short-range exchange energy:
            !           Ex = rho * Eps
            ! Ex_RHO, Ex_SIG, Ex_LAP, Ex_TAU
            !           Derivatives of Ex: dEx/dRho, dEx/dSigma, dEx/dLapl, dEx/dTau
            ! RHO
            !           Total electron density (alpha+beta spin contributions)
            !           of a closed-shell system          
            ! SIGMA
            !           Density gradient = (Nabla RHO) * (Nabla RHO)
            ! LAPL
            !           Density Laplacian = Nabla^2 RHO
            ! TAU
            !           Kinetic energy density = 2 * SUM_i (Nabla Phi_i) * (Nabla Phi_i), where 
            !           the summation is over occupied orbitals. Note that the definition of TAU
            !           does not include the 1/2 factor which some authors use.
            !
            ! SRFRAC
            !           Fraction of the semilocal exchange at short range
            ! OMEGA
            !           Range-separation parameter of the long-range potential erf(omega * r) / r
            !
            real(F64), intent(out) :: eps
            real(F64), intent(out) :: ex_rho
            real(F64), intent(out) :: ex_sig
            real(F64), intent(out) :: ex_lap
            real(F64), intent(out) :: ex_tau
            real(F64), intent(in)  :: rho
            real(F64), intent(in)  :: sigma
            real(F64), intent(in)  :: lapl
            real(F64), intent(in)  :: tau
            real(F64), intent(in)  :: srfrac
            real(F64), intent(in)  :: omega

            real(F64) :: fr_eps, fr_rho, fr_sig, fr_lap, fr_tau
            real(F64) :: lr_eps, lr_rho, lr_sig, lr_lap, lr_tau
            real(F64) :: sr_eps, sr_rho, sr_sig, sr_lap, sr_tau

            call scan_x(fr_eps, fr_rho, fr_sig, fr_tau, rho, sigma, tau)
            fr_lap = ZERO
            call modrzej2016_sr_x_exact_curvature(sr_eps, sr_rho, sr_sig, sr_lap, sr_tau, rho, sigma, lapl, tau, &
                  fr_eps, fr_rho, fr_sig, fr_lap, fr_tau, omega)
            lr_eps = fr_eps - sr_eps
            lr_rho = fr_rho - sr_rho
            lr_sig = fr_sig - sr_sig
            lr_lap = fr_lap - sr_lap
            lr_tau = fr_tau - sr_tau
            eps = srfrac * sr_eps + lr_eps
            ex_rho = srfrac * sr_rho + lr_rho
            ex_sig = srfrac * sr_sig + lr_sig
            ex_lap = srfrac * sr_lap + lr_lap
            ex_tau = srfrac * sr_tau + lr_tau
      end subroutine ec_sh_scan_x
      

      subroutine scan_x(eps, e_rho, e_sigma, e_tau, rho, sigma, tau)
            real(F64), intent(out) :: eps
            real(F64), intent(out) :: e_rho
            real(F64), intent(out) :: e_sigma
            real(F64), intent(out) :: e_tau
            real(F64), intent(in)  :: rho
            real(F64), intent(in)  :: sigma
            real(F64), intent(in)  :: tau
            
            real(F64), parameter :: s_coeff = (TWO * (THREE * PI**2)**(ONE/THREE))**2
            real(F64), parameter :: exUEG_coeff = -THREE/FOUR * (THREE/PI)**(ONE/THREE)
            real(F64), parameter :: tauUEG_coeff = (THREE/TEN) * (THREE*PI**2)**(TWO/THREE)
            real(F64) :: s2, s2_sigma, s2_rho, epsUEG, exUEG, exUEG_rho, tauw, tauw_sigma, tauw_rho
            real(F64) :: alpha, alpha_tau, alpha_sigma, alpha_rho, tauUEG, tauUEG_rho
            real(F64) :: fx, fx_s2, fx_alpha
            
            s2 = sigma / (s_coeff * rho**(EIGHT/THREE))
            s2_sigma = ONE / (s_coeff * rho**(EIGHT/THREE))
            s2_rho = -(EIGHT/THREE) * s2 / rho
            !
            ! UEG exchange energy 
            !
            epsUEG = exUEG_coeff * rho**(ONE/THREE)
            exUEG = exUEG_coeff * rho**(FOUR/THREE)
            exUEG_rho = (FOUR/THREE) * exUEG_coeff * rho**(ONE/THREE)
            !
            ! Weizsacker kinetic energy (alpha+beta spin, including the 1/2 factor)
            !
            tauw = sigma / (EIGHT * rho)
            tauw_sigma = ONE / (EIGHT * rho)
            tauw_rho = -sigma / (EIGHT * rho**2)
            !
            ! UEG kinetic energy (alpha+beta spin, including the 1/2 factor)
            !
            tauUEG = tauUEG_coeff * rho**(FIVE/THREE)
            tauUEG_rho = (FIVE/THREE) * tauUEG_coeff * rho**(TWO/THREE)
            !
            ! The meta-GGA inhomogeneity ingredient.
            ! Tau is multiplied by 1/2.
            !
            alpha = (ONE/TWO * tau - tauw) / tauUEG
            alpha_tau = ONE/TWO / tauUEG
            alpha_sigma = -tauw_sigma / tauUEG
            alpha_rho = -tauw_rho / tauUEG - alpha / tauUEG * tauUEG_rho
            call scan_enhancement_factor(fx, fx_s2, fx_alpha, s2, alpha)
            eps = epsUEG * fx
            e_rho = exUEG_rho * fx + exUEG * (fx_s2 * s2_rho + fx_alpha * alpha_rho)
            e_sigma = exUEG * (fx_s2 * s2_sigma + fx_alpha * alpha_sigma)
            e_tau = exUEG * fx_alpha * alpha_tau
      end subroutine scan_x


      subroutine scan_enhancement_factor(fx, fx_s2, fx_alpha, s2, alpha)
            real(F64), intent(out) :: fx
            real(F64), intent(out) :: fx_s2
            real(F64), intent(out) :: fx_alpha
            real(F64), intent(in)  :: s2
            real(F64), intent(in)  :: alpha

            real(F64) :: h1x, h1x_s2, h1x_alpha
            real(F64) :: gx, gx_s2, s12, f, f_alpha, e1
            real(F64), parameter :: h0x = 1.174_F64
            real(F64), parameter :: a1 = 4.9479_F64
            real(F64), parameter :: c1x = 0.667_F64
            real(F64), parameter :: c2x = 0.8_F64
            real(F64), parameter :: dx = 1.24_F64

            call h1xEq5(h1x, h1x_s2, h1x_alpha, s2, alpha)
            if (s2 > 1.0E-5_F64) then
                  s12 = s2**(ONE/FOUR)
                  e1 = exp(-a1 / s12)
                  gx = ONE - e1
                  gx_s2 = -(ONE/FOUR) * a1 / (s2 * s12) * e1
            else
                  gx = ONE
                  gx_s2 = ZERO
            end if
            call scan_interp_func(f, f_alpha, alpha, c1x, c2x, dx)
            fx = (h1x * (ONE - f) + h0x * f) * gx
            fx_s2 = h1x_s2 * (ONE - f) * gx + (h1x * (ONE - f) + h0x * f) * gx_s2
            fx_alpha = (h1x_alpha * (ONE - f) - h1x * f_alpha + h0x * f_alpha) * gx
      end subroutine scan_enhancement_factor
      

      subroutine h1xEq5(h1x, h1x_s2, h1x_alpha, s2, alpha)
            real(F64), intent(out) :: h1x
            real(F64), intent(out) :: h1x_s2
            real(F64), intent(out) :: h1x_alpha
            real(F64), intent(in)  :: s2
            real(F64), intent(in)  :: alpha

            real(F64), parameter :: k1 = 0.065_F64
            real(F64) :: h1x_x, x, x_s2, x_alpha

            call xEq6(x, x_s2, x_alpha, s2, alpha)
            h1x = ONE + k1 - k1 / (ONE + x / k1)
            h1x_x = ONE / (ONE + x / k1)**2
            h1x_s2 = h1x_x * x_s2
            h1x_alpha = h1x_x * x_alpha
      end subroutine h1xEq5

      
      subroutine xEq6(x, x_s2, x_alpha, s2, alpha)
            real(F64), intent(out) :: x
            real(F64), intent(out) :: x_s2
            real(F64), intent(out) :: x_alpha
            real(F64), intent(in)  :: s2
            real(F64), intent(in)  :: alpha

            real(F64), parameter :: muAK = 10.0_F64/81.0_F64
            real(F64), parameter :: b2 = sqrt(5913.0_F64/405000.0_F64)
            real(F64), parameter :: b1 = (511.0_F64/13500.0_F64)/(2.0_F64*b2)
            real(F64), parameter :: b3 = 0.5_F64
            real(F64), parameter :: k1 = 0.065_F64
            real(F64), parameter :: b4 = muAK**2/k1-1606.0_F64/18225.0_F64-b1**2
            real(F64) :: e1, x1, x1_s2, e2, x2, x2_s2, x2_alpha
            
            e1 = exp(-b4/muAK * s2)
            x1 = muAK * s2 + b4 * s2**2 * e1
            x1_s2 = muAK + TWO * b4 * s2 * e1 - (b4**2/muAK) * s2**2 * e1
            e2 = exp(-b3 * (ONE - alpha)**2)
            x2 = b1 * s2 + b2 * (ONE - alpha) * e2
            x2_s2 = b1
            x2_alpha = -b2 * e2 + TWO * b3 * b2 * (ONE - alpha)**2 * e2
            x = x1 + x2**2
            x_s2 = x1_s2 + TWO * x2 * x2_s2
            x_alpha = TWO * x2 * x2_alpha
      end subroutine xEq6

      
      subroutine scan_interp_func(f, f_alpha, alpha, c1, c2, d)
            !
            ! Eq. 9 in Sun, J., Ruzsinszky, A., Perdew, J.P. Phys. Rev. Lett. 115, 036402 (2015);
            ! doi: 10.1103/PhysRevLett.115.036402
            !
            real(F64), intent(out) :: f
            real(F64), intent(out) :: f_alpha
            real(F64), intent(in)  :: alpha
            real(F64), intent(in)  :: c1
            real(F64), intent(in)  :: c2
            real(F64), intent(in)  :: d

            real(F64) :: t, t_alpha

            if (abs(alpha-ONE) < 0.01_F64) then
                  f = ZERO
                  f_alpha = ZERO
            else if (alpha < ONE) then
                  t = c1 * alpha / (ONE - alpha)
                  t_alpha = c1 / (alpha - ONE)**2
                  f = exp(-t)
                  f_alpha = -f * t_alpha
            else
                  t = c2 / (ONE - alpha)
                  t_alpha = c2 / (alpha - ONE)**2
                  f = -d * exp(t)
                  f_alpha = f * t_alpha
            end if
      end subroutine scan_interp_func
end module scan_x_energy
