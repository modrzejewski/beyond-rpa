module pbe_x_energy
      use arithmetic
      use math_constants
      use modrzej2016_x_energy

      implicit none

      real(F64), parameter :: PBE_X_RHO_THRESH = 1.0E-10_F64

contains
      
      subroutine ec_sr_pbe_x(sr_eps, sr_rho, sr_sig, sr_lap, sr_tau, rho, sigma, lapl, tau, omega)
            ! --------------------------------------------------------------------------------------
            ! Compute the short-range PBE exchange energy using the meta-GGA Becke-Roussel-type
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

            call pbe_x(fr_eps, fr_rho, fr_sig, rho, sigma)
            fr_lap = ZERO
            fr_tau = ZERO
            call modrzej2016_sr_x_exact_curvature(sr_eps, sr_rho, sr_sig, sr_lap, sr_tau, rho, sigma, lapl, tau, &
                  fr_eps, fr_rho, fr_sig, fr_lap, fr_tau, omega)
      end subroutine ec_sr_pbe_x


      subroutine ec_sr_pbesol_x(sr_eps, sr_rho, sr_sig, sr_lap, sr_tau, rho, sigma, lapl, tau, omega)
            ! -------------------------------------------------------------------------------------
            ! Compute the short-range PBEsol exchange energy using the meta-GGA Becke-Roussel-type
            ! exchange hole. 
            ! -------------------------------------------------------------------------------------
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

            call pbesol_x(fr_eps, fr_rho, fr_sig, rho, sigma)
            fr_lap = ZERO
            fr_tau = ZERO
            call modrzej2016_sr_x_exact_curvature(sr_eps, sr_rho, sr_sig, sr_lap, sr_tau, rho, sigma, lapl, tau, &
                  fr_eps, fr_rho, fr_sig, fr_lap, fr_tau, omega)
      end subroutine ec_sr_pbesol_x


      subroutine ec_sh_pbe_x(eps, ex_rho, ex_sig, ex_lap, ex_tau, rho, sigma, lapl, tau, srfrac, omega)
            ! --------------------------------------------------------------------------------------
            ! Compute the semilocal part of the screened-hybrid PBE exchange energy using
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

            call pbe_x(fr_eps, fr_rho, fr_sig, rho, sigma)
            fr_tau = ZERO
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
      end subroutine ec_sh_pbe_x
      

      subroutine pbe_x(eps, e_rho, e_sigma, rho, sigma)
            !
            ! Compute the PBE exchange energy using the mu parameter
            ! with extra digits of accuracy beyond those provided in Ref. 1.
            !
            ! 1. Perdew, J.P., Burke, K., Ernzerhof, M., Phys. Rev. Lett. 77, 3865 (1996);
            !    doi: 10.1103/PhysRevLett.77.3865
            !
            real(F64), intent(out) :: eps
            real(F64), intent(out) :: e_rho
            real(F64), intent(out) :: e_sigma
            real(F64), intent(in)  :: rho
            real(F64), intent(in)  :: sigma
            !
            ! Coefficient in the gradient expansion for correlation
            ! with extra digits of accuracy taken from K. Burke's 
            ! Fortran subroutine
            !
            real(F64), parameter :: betaPBE = 0.06672455060314922_F64
            real(F64), parameter :: muPBE = betaPBE * PI**2 / THREE

            call pbe_x_template(eps, e_rho, e_sigma, rho, sigma, muPBE)
      end subroutine pbe_x


      subroutine pbesol_x(eps, e_rho, e_sigma, rho, sigma)
            !
            ! Compute the PBEsol exchange energy
            !
            ! 1. Perdew, J.P., Ruzsinszky, A., Csonka, G.I., Vydrov, O.A., Scuseria, G.E.,
            !    Constantin, L.A., Zhou, X., Burke, K., Phys. Rev. Lett. 100, 136406 (2008);
            !    doi: 10.1103/PhysRevLett.100.136406
            !
            real(F64), intent(out) :: eps
            real(F64), intent(out) :: e_rho
            real(F64), intent(out) :: e_sigma
            real(F64), intent(in)  :: rho
            real(F64), intent(in)  :: sigma

            real(F64), parameter :: muPBEsol = 10.0_F64 / 81.0_F64

            call pbe_x_template(eps, e_rho, e_sigma, rho, sigma, muPBEsol)
      end subroutine pbesol_x


      subroutine pbe_x_template(eps, e_rho, e_sigma, rho, sigma, muGGA)
            !
            ! Compute the PBE exchange energy for a given value of the parameter mu.
            ! 1. Perdew, J.P., Burke, K., Ernzerhof, M., Phys. Rev. Lett. 77, 3865 (1996);
            !    doi: 10.1103/PhysRevLett.77.3865
            !
            real(F64), intent(out) :: eps
            real(F64), intent(out) :: e_rho
            real(F64), intent(out) :: e_sigma
            real(F64), intent(in)  :: rho
            real(F64), intent(in)  :: sigma
            real(F64), intent(in)  :: muGGA
            
            real(F64) :: epslda, epslda_rho, p_denom, p, p_rho, p_sigma
            real(F64) :: fxGGA, fxGGA_p, fxGGA_rho, fxGGA_sigma, rho83
            real(F64), parameter :: kappa = 0.804_F64

            rho83 = rho**(EIGHT/THREE)
            !
            ! LDA exchange
            !
            epslda = (-THREE/FOUR)*(THREE/PI)**(ONE/THREE) * rho**(ONE/THREE)
            epslda_rho = ONE/THREE * epslda / rho
            !
            ! Reduced gradient
            !
            p_denom = FOUR*(THREE*PI**2)**(TWO/THREE) * rho83
            p = sigma / p_denom
            p_rho = -EIGHT/THREE * p / rho
            p_sigma = ONE / p_denom

            fxGGA = ONE + kappa - kappa / (ONE + muGGA * p / kappa)
            fxGGA_p = muGGA / (ONE + muGGA * p / kappa)**2
            fxGGA_rho = fxGGA_p * p_rho
            fxGGA_sigma = fxGGA_p * p_sigma
            
            eps = epslda * fxgga
            e_rho = eps + rho * (epslda_rho * fxGGA + epslda * fxGGA_rho)
            e_sigma = rho * epslda * fxGGA_sigma
      end subroutine pbe_x_template
end module pbe_x_energy
