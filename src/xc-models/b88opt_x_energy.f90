module b88opt_x_energy
      use arithmetic
      use math_constants

      implicit none

      real(F64), parameter :: B88OPT_X_RHO_THRESH = 1.0E-10_F64
      
contains

      pure subroutine b88opt_enhancement_factor(fx, fx_rho, fx_sigma, rho, sigma)
            !
            ! Compute the B88opt exchange enhancement factor for a spin-compensated
            ! system. 
            !
            ! Check if the electron density is non-zero before calling this subroutine.
            ! This subroutine accepts density gradients which are exactly zero.
            !
            ! The correctness of this code was checked against Mathemetica.
            !
            ! 1. Klimes, J., Bowler, D.R., and Michaelides, A., J. PHys.: Condens. Matter 22,
            !    022201 (2010); doi:10.1088/0953-8984/22/2/022201
            ! 2. Becke, A.D., Phys. Rev. A 6, 3098 (1988); doi: 10.1103/PhysRevA.38.3098
            !
            real(F64), intent(out) :: fx
            real(F64), intent(out) :: fx_rho
            real(F64), intent(out) :: fx_sigma
            real(F64), intent(in)  :: rho
            real(F64), intent(in)  :: sigma

            real(F64) :: t, x, x_rhoa, w, w_x, b, b_x, b_rhoa, b_sigaa
            real(F64) :: arcsinhx, rhoa13, rhoa43, x_xsig, rho_a, sigma_aa
            !
            ! Parameters defined below Eq. 3 in Ref. 1
            !
            real(F64), parameter :: muKlimes = 0.22_F64
            real(F64), parameter :: betaKlimes = muKlimes / 1.2_F64
            real(F64), parameter :: cKlimes = TWO**(FOUR/THREE)*(THREE*PI**2)**(ONE/THREE)
            real(F64), parameter :: beta = betaKlimes / (SIX*cKlimes)
            real(F64), parameter :: c_gga = muKlimes / (cKlimes**2)
            
            sigma_aa = abs(sigma) / FOUR
            rho_a = rho / TWO
            t = sqrt(sigma_aa)
            !
            ! t_sig = ONE / (TWO * t)
            !
            rhoa13 = rho_a**(ONE/THREE)
            rhoa43 = rhoa13 * rho_a
            !
            ! Reduced density gradient, Eq. 4 in Ref. 2
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
            ! The x-dependent part of Eq. 8 in Ref. 2
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
      end subroutine b88opt_enhancement_factor

      
      pure subroutine b88opt_x(eps, e_rho, e_sigma, rho, sigma)
            !
            ! Compute the B88opt exchange energy for a spin-compensated system. 
            !
            ! Check if the electron density is non-zero before calling this subroutine.
            ! This subroutine accepts density gradients which are exactly zero.
            !
            ! The correctness of this code was checked against Mathemetica.
            !
            ! 1. Klimes, J., Bowler, D.R., and Michaelides, A., J. PHys.: Condens. Matter 22,
            !    022201 (2010); doi:10.1088/0953-8984/22/2/022201
            ! 2. Becke, A.D., Phys. Rev. A 6, 3098 (1988); doi: 10.1103/PhysRevA.38.3098
            !
            real(F64), intent(out) :: eps
            real(F64), intent(out) :: e_rho
            real(F64), intent(out) :: e_sigma
            real(F64), intent(in)  :: rho
            real(F64), intent(in)  :: sigma
            
            real(F64) :: epslda, fx, fx_rho, fx_sigma

            call b88opt_enhancement_factor(fx, fx_rho, fx_sigma, rho, sigma)
            epslda = (-THREE/FOUR)*(THREE/PI)**(ONE/THREE) * rho**(ONE/THREE)
            eps = epslda * fx
            e_rho = (FOUR/THREE) * epslda * fx + rho * epslda * fx_rho
            e_sigma = rho * epslda * fx_sigma
      end subroutine b88opt_x
end module b88opt_x_energy
