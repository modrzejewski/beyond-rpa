module b88_x_energy
      use arithmetic
      use math_constants
      use modrzej2016_x_energy

      implicit none

      real(F64), parameter :: B88_X_RHO_THRESH = 1.0E-10_F64
      
contains

      pure subroutine b88_enhancement_factor(fx, fx_rho, fx_sigma, rho, sigma)
            !
            ! Compute the B88 exchange enhancement factor for a spin-compensated
            ! system. 
            !
            ! Check if the electron density is non-zero before calling this subroutine.
            ! This subroutine accepts density gradients which are exactly zero.
            !
            ! The correctness of this code was checked against Mathemetica.
            !
            ! 1. Becke, A.D., Phys. Rev. A 6, 3098 (1988); doi: 10.1103/PhysRevA.38.3098
            !
            real(F64), intent(out) :: fx
            real(F64), intent(out) :: fx_rho
            real(F64), intent(out) :: fx_sigma
            real(F64), intent(in)  :: rho
            real(F64), intent(in)  :: sigma

            real(F64) :: t, x, x_rhoa, w, w_x, b, b_x, b_rhoa, b_sigaa
            real(F64) :: arcsinhx, rhoa13, rhoa43, x_xsig, rho_a, sigma_aa
            !
            ! Beta defined in Ref. 1 via a least-squares fit to the exchange
            ! energies of noble gas atoms
            !
            real(F64), parameter :: beta = 0.0042_F64
            real(F64), parameter :: c_gga = TWO*SIX**(TWO/THREE)*beta*PI**(ONE/THREE)/NINE
            
            sigma_aa = abs(sigma) / FOUR
            rho_a = rho / TWO
            t = sqrt(sigma_aa)
            !
            ! t_sig = ONE / (TWO * t)
            !
            rhoa13 = rho_a**(ONE/THREE)
            rhoa43 = rhoa13 * rho_a
            !
            ! Reduced density gradient, Eq. 4 in Ref. 1
            !
            x = t / rhoa43
            x_rhoa = -(FOUR/THREE) * x / rho_a
            !
            ! Introduce a new variable (x * dx/dsigma) to avoid division by zero when
            ! sigma is exactly zero.
            ! x * dx/dsigma = (t / rho43) * (1 / (2*t) / rho43) = 1 / (2 * rho43 * rho43)
            !
            x_xsig = ONE/TWO / (rhoa43 * rhoa43)
            arcsinhx = asinh(x)
            w = x * arcsinhx
            w_x = x / sqrt(ONE + x**2) + arcsinhx
            !
            ! The x-dependent part of Eq. 8 in Ref. 1
            !
            b = x**2 / (ONE + SIX*beta * w)
            b_x = TWO * x / (ONE + SIX*beta * w) - x**2 / (ONE + SIX*beta * w)**2 * SIX*beta * w_x
            !
            ! b_sig = b_x * dx/dsigma_aa
            !
            b_sigaa = TWO * x_xsig / (ONE + SIX*beta * w) - x * x_xsig / (ONE + SIX*beta * w)**2 * SIX*beta * w_x
            b_rhoa = b_x * x_rhoa
            !
            ! Use the alpha-spin components to compute the enhancement factor
            ! of the spin-compensated functional
            !
            fx = ONE + c_gga * b
            fx_rho = c_gga * b_rhoa * (ONE/TWO)
            fx_sigma = c_gga * b_sigaa * (ONE/FOUR)
      end subroutine b88_enhancement_factor

      
      pure subroutine b88_x(eps, e_rho, e_sigma, rho, sigma)
            !
            ! Compute the B88 exchange energy for a spin-compensated system. 
            !
            ! Check if the electron density is non-zero before calling this subroutine.
            ! This subroutine accepts density gradients which are exactly zero.
            !
            ! The correctness of this code was checked against Mathemetica.
            !
            ! 1. Becke, A.D., Phys. Rev. A 6, 3098 (1988); doi: 10.1103/PhysRevA.38.3098
            !
            real(F64), intent(out) :: eps
            real(F64), intent(out) :: e_rho
            real(F64), intent(out) :: e_sigma
            real(F64), intent(in)  :: rho
            real(F64), intent(in)  :: sigma
            
            real(F64) :: epslda, fx, fx_rho, fx_sigma

            call b88_enhancement_factor(fx, fx_rho, fx_sigma, rho, sigma)
            epslda = (-THREE/FOUR)*(THREE/PI)**(ONE/THREE) * rho**(ONE/THREE)
            eps = epslda * fx
            e_rho = (FOUR/THREE) * epslda * fx + rho * epslda * fx_rho
            e_sigma = rho * epslda * fx_sigma
      end subroutine b88_x


      subroutine ec_sr_b88_x(sr_eps, sr_rho, sr_sig, sr_lap, sr_tau, rho, sigma, lapl, tau, omega)
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

            call b88_x(fr_eps, fr_rho, fr_sig, rho, sigma)
            fr_lap = ZERO
            fr_tau = ZERO
            call modrzej2016_sr_x_exact_curvature(sr_eps, sr_rho, sr_sig, sr_lap, sr_tau, rho, sigma, lapl, tau, &
                  fr_eps, fr_rho, fr_sig, fr_lap, fr_tau, omega)
      end subroutine ec_sr_b88_x
end module b88_x_energy
