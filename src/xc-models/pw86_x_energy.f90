module pw86_x_energy
      use arithmetic
      use math_constants
      
      implicit none

      real(F64), parameter :: PW86_X_RHO_THRESH = 1.0E-10_F64
      !
      ! Parameters of the PW86 exchange functional (Table 2 in Murray et al.
      ! J. Chem. Theory Comput. 5, 2574 (2009); doi: 10.1021/ct900365q
      !
      real(F64), parameter :: PW86_A = 0.0864_F64
      real(F64), parameter :: PW86_B = 14.0_F64
      real(F64), parameter :: PW86_C = 0.2_F64

      real(F64), parameter :: RPW86_A = 0.1234_F64
      real(F64), parameter :: RPW86_B = 17.33_F64
      real(F64), parameter :: RPW86_C = 0.163_F64

contains

      pure subroutine pw86_x(eps, vrho, vsigma, rho, sigma)
            !
            ! Original PW86 exchange, as defined in Perdew, J.P. and Wang, Y.
            ! Phys. Rev. B 33, 8800(R) (1986); doi: 10.1103/PhysRevB.33.8800
            !
            real(F64), intent(out) :: eps
            real(F64), intent(out) :: vrho
            real(F64), intent(out) :: vsigma
            real(F64), intent(in)  :: rho
            real(F64), intent(in)  :: sigma

            call pw86_x_template(eps, vrho, vsigma, rho, sigma, PW86_A, PW86_B, PW86_C)
      end subroutine pw86_x

      
      pure subroutine rpw86_x(eps, vrho, vsigma, rho, sigma)
            !
            ! PW86 exchange refitted by Murray et al., as defined in Table 2 of 
            ! Murray et al. J. Chem. Theory Comput. 5, 2574 (2009);
            ! doi: 10.1021/ct900365q
            !
            real(F64), intent(out) :: eps
            real(F64), intent(out) :: vrho
            real(F64), intent(out) :: vsigma
            real(F64), intent(in)  :: rho
            real(F64), intent(in)  :: sigma

            call pw86_x_template(eps, vrho, vsigma, rho, sigma, RPW86_A, RPW86_B, RPW86_C)
      end subroutine rpw86_x
      

      pure subroutine pw86_x_template(eps, vrho, vsigma, rho, sigma, a, b, c)
            ! --------------------------------------------------------------------------
            ! Compute the exchange energy density and first derivatives of the PW86
            ! exchange functional. This is a spin-unpolarized version of the functional.
            ! Use the exact spin relation to get the spin-polarized energies
            ! and derivatives: [3]
            !
            ! Ex[\rho_\alpha, \rho_\beta] = 1/2*(Ex[2 \rho_\alpha] + Ex[2 \rho_\beta]),
            !
            ! where the functionals on the right-hand side are closed shell.
            !
            ! Check if the electron density is non-zero before calling this subroutine.
            ! This subroutine accepts density gradients which are exactly zero.
            ! --------------------------------------------------------------------------
            ! 1. Perdew, J.P. and Wang, Y. Phys. Rev. B 33, 8800(R) (1986);
            !    doi: 10.1103/PhysRevB.33.8800
            ! 2. Murray et al. J. Chem. Theory Comput. 5, 2574 (2009);
            !    doi: 10.1021/ct900365q
            ! 3. Oliver, G.L. and Perdew, J.P., Spin-density gradient expansion for
            !    the kinetic energy, Phys. Rev. A 20, 397; doi: 10.1103/PhysRevA.20.397
            ! --------------------------------------------------------------------------
            ! EPS    
            !        Density of the exchange energy
            ! VRHO   
            !        d(RHO*EPS) / dRHO
            ! VSIGMA 
            !        d(RHO*EPS) / dSIGMA
            ! RHO    
            !        Total electron density (alpha + beta spins)
            ! SIGMA  
            !        Square of the gradient of the total electron density (Nabla Rho)**2
            ! A, B, C
            !        Numerical parameters of the PW86 exchange energy
            !        
            !
            real(F64), intent(out) :: eps
            real(F64), intent(out) :: vrho
            real(F64), intent(out) :: vsigma
            real(F64), intent(in)  :: rho
            real(F64), intent(in)  :: sigma
            real(F64), intent(in)  :: a, b, c
            
            real(F64) :: rho13, rho23, rho43, rho83, rho2
            real(F64) :: s, s_sigma, s_rho
            real(F64) :: ex_unif, ex_unif_rho
            real(F64) :: fx, fx_t, fx_s, fx_rho, fx_sigma
            real(F64) :: t, t_s
            real(F64), parameter :: s_coeff = (TWO * (THREE * PI**2)**(ONE/THREE))**2
            real(F64), parameter :: ex_unif_coeff = -THREE/FOUR * (THREE/PI)**(ONE/THREE)
            real(F64), parameter :: m = 1.0_F64 / 15.0_F64


            rho2 = rho**2
            rho13 = rho**(ONE/THREE)
            rho23 = rho13**2
            rho43 = rho * rho13
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
            ! Eq. 11 in Ref. 2
            !
            t = ONE + FIFTEEN * a * s + b * s**2 + c * s**3
            t_s = FIFTEEN * a + TWO * b * s + THREE * c * s**2
            fx = t**m
            fx_t = m * fx / t
            fx_s = fx_t * t_s
            fx_rho = fx_s * s_rho
            fx_sigma = fx_s * s_sigma
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
      end subroutine pw86_x_template
end module pw86_x_energy
