module lda_x_energy
      use arithmetic
      use math_constants
      use modrzej2016_x_energy

      implicit none

      real(F64), parameter :: LDA_X_RHO_THRESH = 1.0E-10_F64

contains

      subroutine ec_sr_lda_x(sr_eps, sr_rho, sr_sig, sr_lap, sr_tau, rho, sigma, lapl, tau, omega)
            ! --------------------------------------------------------------------------------------
            ! Compute the short-range LDA exchange energy using the meta-GGA Becke-Roussel-type
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

            fr_eps = (-THREE/FOUR)*(THREE/PI)**(ONE/THREE) * rho**(ONE/THREE)
            fr_rho = FOUR/THREE * fr_eps
            fr_sig = ZERO
            fr_lap = ZERO
            fr_tau = ZERO
            call modrzej2016_sr_x_exact_curvature(sr_eps, sr_rho, sr_sig, sr_lap, sr_tau, rho, sigma, lapl, tau, &
                  fr_eps, fr_rho, fr_sig, fr_lap, fr_tau, omega)
      end subroutine ec_sr_lda_x


      pure subroutine sr_lda_x(sr_eps, rho, omega)
            real(F64), intent(out) :: sr_eps
            real(F64), intent(in)  :: rho
            real(F64), intent(in)  :: omega

            real(F64) :: fx_lr, fx_lr_rho, lda_lr_eps, lda_fr_eps

            call long_range_ldax(fx_lr, fx_lr_rho, rho, omega)
            lda_fr_eps = (-THREE/FOUR)*(THREE/PI)**(ONE/THREE) * rho**(ONE/THREE)
            lda_lr_eps = fx_lr * lda_fr_eps
            sr_eps = lda_fr_eps - lda_lr_eps
      end subroutine sr_lda_x

      
      pure subroutine long_range_ldax(fx, fx_rho, rho, omega)
            ! ------------------------------------------------------------------
            ! Compute the enhancement factor of the long-range (LR) component
            ! of the LDA exchange functional. The LR exchange corresponds to
            ! the integral of the exact LDA exchange hole with ERF(omega*r).
            ! All equations are coded according to the paper of Gill et al.
            ! Fx is related to the total exchange energy of a spin-compensated
            ! system as follows:
            ! Ex = Int rho * Fx * epsLDA
            ! epsLDA = (-3/4) * (3/Pi)**(1/3) rho**(1/3)
            ! where rho is the total (alpha+beta) density. 
            ! ------------------------------------------------------------------
            ! 1. Gill, P.M.W., Adamson, R.D., Pople, J.A., Coulomb-attenuated
            !    exchange energy density functionals, Mol. Phys. 88, 105 (1996);
            !    doi: 10.1080/00268979609484488
            ! ------------------------------------------------------------------
            ! Fx
            !          Enhancement factor
            ! Fx_rho
            !          dFx/drho
            ! rho
            !          Electron density (alpha + beta spin)
            ! omega
            !          Range separation parameter
            !
            real(F64), intent(out) :: fx
            real(F64), intent(out) :: fx_rho
            real(F64), intent(in)  :: rho
            real(F64), intent(in)  :: omega

            real(F64) :: kf, lambda, lambda_rho, erf_l, exp_l
            real(F64) :: fx_lambda, l2, l3
            real(F64) :: r0, r2, r4, r6, r8, r10, s0, s2, s4, s6, s8, s10
            real(F64) :: t3, t5, t7, t9, u0, u2, u4, u6, u8, u10

            real(F64), parameter :: a0  = 1.0000000000000000_F64
            real(F64), parameter :: a2  = 0.2299813934844096_F64
            real(F64), parameter :: a4  = 0.02865043745818842_F64
            real(F64), parameter :: a6  = 0.001687153141995639_F64
            real(F64), parameter :: a8  = 0.00005869469097965848_F64
            real(F64), parameter :: a10 = 3.915747347574243E-7_F64

            real(F64), parameter :: b0  = 1.0000000000000000_F64
            real(F64), parameter :: b2  = 0.3410925045955208_F64
            real(F64), parameter :: b4  = 0.04988293796880184_F64
            real(F64), parameter :: b6  = 0.003925779109556212_F64
            real(F64), parameter :: b8  = 0.000166993105972281_F64
            real(F64), parameter :: b10 = 3.085184080717404E-6_F64

            real(F64), parameter :: c3  = 0.2222222222222222_F64
            real(F64), parameter :: c5  = 0.002970325500527002_F64
            real(F64), parameter :: c7  = 0.001180477937481607_F64
            real(F64), parameter :: c9  = -0.0001344107095646778_F64

            real(F64), parameter :: d0  = 1.0000000000000000_F64
            real(F64), parameter :: d2  = 0.3133664647523715_F64
            real(F64), parameter :: d4  = 0.0350363758586644_F64
            real(F64), parameter :: d6  = 0.00087218865587407_F64
            real(F64), parameter :: d8  = -0.0001322088663117161_F64
            real(F64), parameter :: d10 = -9.106702917223843E-6_F64

            kf = (THREE*PI**2 * rho)**(ONE/THREE)
            lambda = omega / kf
            lambda_rho = -ONE/THREE * lambda / rho

            if (lambda < 0.1_F64) then
                  fx = TWO/THREE * lambda * (TWO*sqrt(PI) - THREE * lambda + lambda**3)
                  fx_lambda = FOUR/THREE * (sqrt(PI) - THREE * lambda + TWO * lambda**3)
            else if (lambda < 1.7_F64) then
                  l2 = lambda**2
                  l3 = lambda**3
                  erf_l = erf(ONE/lambda)
                  exp_l = exp(-ONE/l2)
                  fx = TWO/THREE * lambda * (TWO*sqrt(PI) * erf_l - THREE * lambda + l3 + (TWO * lambda - l3) * exp_l)
                  fx_lambda = FOUR/THREE * (lambda * exp_l - THREE * lambda + TWO * l3 * (ONE - exp_l) + sqrt(PI) * erf_l)
            else
                  l2 = ONE / lambda**2
                  r10 = l2 * a10
                  r8 = l2 * (a8 + r10)
                  r6 = l2 * (a6 + r8)
                  r4 = l2 * (a4 + r6)
                  r2 = l2 * (a2 + r4)
                  r0 = a0 + r2

                  s10 = l2 * b10
                  s8 = l2 * (b8 + s10)
                  s6 = l2 * (b6 + s8)
                  s4 = l2 * (b4 + s6)
                  s2 = l2 * (b2 + s4)
                  s0 = b0 + s2

                  fx = r0 / s0

                  t9 = l2 * c9
                  t7 = l2 * (c7 + t9)
                  t5 = l2 * (c5 + t7)
                  t3 = l2 * (c3 + t5)
                  t3 = ONE / lambda * t3

                  u10 = l2 * d10
                  u8 = l2 * (d8 + u10)
                  u6 = l2 * (d6 + u8)
                  u4 = l2 * (d4 + u6)
                  u2 = l2 * (d2 + u4)
                  u0 = d0 + u2

                  fx_lambda = t3 / u0
            end if

            fx_rho = fx_lambda * lambda_rho
      end subroutine long_range_ldax
end module lda_x_energy
