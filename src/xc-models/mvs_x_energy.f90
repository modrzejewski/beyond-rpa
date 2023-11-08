!
! "Meta-GGA made very simple" exchange functional of Sun, Perdew, and Ruzsinszky
! PNAS 112, 685 (2015); doi: 10.1073/pnas.1423145112
!
module mvs_x_energy
      use arithmetic
      use math_constants
      use modrzej2016_x_energy

      implicit none

      real(F64), parameter :: MVS_X_RHO_THRESH = 1.0E-10_F64

contains

      subroutine ec_sr_mvs_x(sr_eps, sr_rho, sr_sig, sr_lap, sr_tau, rho, sigma, lapl, tau, omega)
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

            call mvs_x(fr_eps, fr_rho, fr_sig, fr_tau, rho, sigma, tau)
            fr_lap = ZERO
            call modrzej2016_sr_x_exact_curvature(sr_eps, sr_rho, sr_sig, sr_lap, sr_tau, rho, sigma, lapl, tau, &
                  fr_eps, fr_rho, fr_sig, fr_lap, fr_tau, omega)
      end subroutine ec_sr_mvs_x
      

      pure subroutine mvs_x(eps, vrho, vsigma, vtau, rho, sigma, tau)
            ! --------------------------------------------------------------------------
            ! Compute the exchange energy density and first derivatives of the "meta-
            ! GGA made very simple" functional of Sun, Perdew, and Ruzsinszky. [1]
            ! This is a spin-unpolarized version of the functional. Use the exact spin
            ! scaling relation to get the spin-polarized energies and derivatives: [2]
            !
            ! Ex[\rho_\alpha, \rho_\beta] = 1/2*(Ex[2 \rho_\alpha] + Ex[2 \rho_\beta]),
            !
            ! where the functionals on the right-hand side are closed shell.
            !
            ! Check if the electron density is non-zero before calling this subroutine.
            ! This subroutine accepts density gradients which are exactly zero.
            ! This code was checked against a simple python representation
            ! of the functional.
            ! --------------------------------------------------------------------------
            ! 1. Sun, J., Perdew, J. P., and Ruzsinszky, A., Semilocal density
            !    functional obeying a strongly tightened bound for exchange.
            !    PNAS 112, 685 (2015); doi: 10.1073/pnas.1423145112
            ! 2. Oliver, G.L. and Perdew, J.P., Spin-density gradient expansion for
            !    the kinetic energy, Phys. Rev. A 20, 397; doi: 10.1103/PhysRevA.20.397
            ! --------------------------------------------------------------------------
            ! EPS    
            !        Density of the exchange energy
            ! VRHO   
            !        d(RHO*EPS) / dRHO
            ! VSIGMA 
            !        d(RHO*EPS) / dSIGMA
            ! VTAU   
            !        d(RHO*EPS) / dTAU
            ! RHO    
            !        Total electron density (alpha + beta spins)
            ! SIGMA  
            !        Square of the gradient of the total electron density (Nabla Rho)**2
            ! TAU    
            !        Kinetic energy density (alpha+beta spins) without the 1/2 factor
            !        tau = sum_{s=alpha,beta} sum_i (Nabla psi_{i,s}) (Nabla psi_{i,s})
            !
            real(F64), intent(out) :: eps
            real(F64), intent(out) :: vrho
            real(F64), intent(out) :: vsigma
            real(F64), intent(out) :: vtau
            real(F64), intent(in)  :: rho
            real(F64), intent(in)  :: sigma
            real(F64), intent(in)  :: tau
            
            real(F64) :: rho13, rho23, rho43, rho53, rho83, rho2
            real(F64) :: s, s_sigma, s_rho
            real(F64) :: ex_unif, ex_unif_rho
            real(F64) :: tauw, tauw_sigma, tauw_rho
            real(F64) :: tau_unif, tau_unif_rho
            real(F64) :: alpha, alpha2, alpha_tau, alpha_sigma, alpha_rho
            real(F64) :: gx, gx_alpha, gx_rho, gx_tau, gx_sigma
            real(F64) :: u, v, v_alpha, v14
            real(F64) :: w, w18, w_s
            real(F64) :: fx, fx_gx, fx_w, fx_rho, fx_sigma, fx_tau
            real(F64), parameter :: s_coeff = (TWO * (THREE * PI**2)**(ONE/THREE))**2
            real(F64), parameter :: ex_unif_coeff = -THREE/FOUR * (THREE/PI)**(ONE/THREE)
            real(F64), parameter :: tau_unif_coeff = (THREE/TEN) * (THREE*PI**2)**(TWO/THREE)
            real(F64), parameter :: k0 = 0.174_F64
            real(F64), parameter :: h0x = ONE + k0
            real(F64), parameter :: h1x = ONE
            real(F64), parameter :: muGE = 10.0_F64/81.0_F64
            !
            ! One unpublished digit of precision for the e1 constant is taken
            ! from Sun's Fortran subroutine (private communication).
            ! The original published value is e1=-1.6665.
            !
            real(F64), parameter :: e1 = -1.66651_F64
            !
            ! Analytic formula for c1 is used instead of c1=0.7438
            ! for consistency with Sun's Fortran subroutine
            !
            real(F64), parameter :: c1 = (20.0_F64*k0/(27.0_F64*muGE))**FOUR-(ONE+e1)**2
            real(F64), parameter :: b = 0.0233_F64

            rho2 = rho**2
            rho13 = rho**(ONE/THREE)
            rho23 = rho13**2
            rho43 = rho * rho13
            rho53 = rho * rho23
            rho83 = rho43**2

            s = sigma / (s_coeff * rho83)
            s_sigma = ONE / (s_coeff * rho83)
            s_rho = -EIGHT/THREE * s / rho
            !
            ! UEG exchange energy 
            !
            ex_unif = ex_unif_coeff * rho13
            ex_unif_rho = ONE/THREE * ex_unif_coeff / rho23
            !
            ! Weizsacker kinetic energy (alpha+beta spin, including the 1/2 factor)
            !
            tauw = sigma / (EIGHT * rho)
            tauw_sigma = ONE / (EIGHT * rho)
            tauw_rho = -sigma / (EIGHT * rho2)
            !
            ! UEG kinetic energy (alpha+beta spin, including the 1/2 factor)
            !
            tau_unif = tau_unif_coeff * rho53
            tau_unif_rho = FIVE/THREE * tau_unif_coeff * rho23
            !
            ! The meta-GGA inhomogeneity ingredient defined
            ! in Eq. 1. The TAU input variable is multiplied by 1/2.
            !
            alpha = (ONE/TWO * tau - tauw) / tau_unif
            alpha_tau = ONE/TWO / tau_unif
            alpha_sigma = -tauw_sigma / tau_unif
            alpha_rho = -tauw_rho / tau_unif - alpha / tau_unif * tau_unif_rho
            !
            ! Alpha-dependent interpolation function
            !
            alpha2 = alpha**2
            u = ONE - alpha
            v = alpha2 * ((c1 + e1**2) * alpha2 + TWO * e1) + ONE
            v_alpha = FOUR * alpha * ((c1 + e1**2) * alpha2 + e1)
            v14 = v**(ONE/FOUR)
            gx = u / v14
            gx_alpha = -ONE / v14 - ONE/FOUR * gx / v * v_alpha
            gx_rho = gx_alpha * alpha_rho
            gx_sigma = gx_alpha * alpha_sigma
            gx_tau = gx_alpha * alpha_tau
            !
            ! Enhancement factor defined in Eq. 7
            !
            w = ONE + b * s**2
            w18 = w**(ONE/EIGHT)
            w_s = TWO * b * s
            fx = (h1x + gx * (h0x - h1x)) / w18
            fx_gx = (h0x - h1x) / w18
            fx_w = -ONE/EIGHT * fx / w
            fx_rho = fx_gx * gx_rho + fx_w * w_s * s_rho
            fx_sigma = fx_gx * gx_sigma + fx_w * w_s * s_sigma
            fx_tau = fx_gx * gx_tau
            !
            ! Exchange energy density (exchange energy per electron)
            !
            eps = ex_unif * fx
            !
            ! d(rho*eps)/drho
            !
            vrho = eps + rho * (ex_unif_rho * fx + ex_unif * fx_rho)
            !
            ! d(rho*eps)/dsigma
            !
            vsigma = rho * ex_unif * fx_sigma
            !
            ! d(rho*eps)/dtau
            !
            vtau = rho * ex_unif * fx_tau
      end subroutine mvs_x
end module mvs_x_energy
