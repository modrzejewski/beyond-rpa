!
! Range-separated exchange functionals developed by Henderson, Janesko, and Scuseria
! (HJS) [1]. The HJS exchange is included in the LRC-wPBEh short-range hybrid
! functional of Rohrdanz et al. [2]. The short-range B88 exchange is defined in Ref. 3.
!
! 1. Henderson, Thomas M., Janesko, Benjamin G., and Scuseria, Gustavo E.,
!    Generalized gradient approximation model exchange holes for range-
!    separated hybrids, J. Chem. Phys. 128, 194105 (2008); doi: 10.1063/1.2921797
!
! 2. Rohrdanz, M., Martins, K., and Herbert, J., A long-range-corrected
!    density functional that performs well for both ground-state
!    properties and time-dependent density functional theory excitation
!    energies, including charge-transfer excited states, J. Chem. Phys. 130, 054112 (2009);
!    doi: 10.1063/1.3073302
!
! 3. Weintraub, E., Henderson, T.M., Scuseria, G.E., J. Chem. Theory Comput. 5, 754 (2009);
!    doi: 10.1021/ct800530u
!
module hjs_x_energy
      use math_constants
      use arithmetic
      use pbe_x_energy

      implicit none
      !
      ! Numerical parameters for PBE and PBEsol (Table 2 in Ref. 1).
      ! To evaluate a chosen variant of the short-range exchange, provide
      ! the appropriate set of parameters to the subroutine HJS_X_TEMPLATE.
      !
      real(F64), parameter :: PBE_A2 =  0.0159941_F64
      real(F64), parameter :: PBE_A3 =  0.0852995_F64
      real(F64), parameter :: PBE_A4 = -0.160368_F64
      real(F64), parameter :: PBE_A5 =  0.152645_F64
      real(F64), parameter :: PBE_A6 = -0.0971263_F64
      real(F64), parameter :: PBE_A7 =  0.0422061_F64

      real(F64), parameter :: PBE_B1 =   5.33319_F64
      real(F64), parameter :: PBE_B2 = -12.4780_F64
      real(F64), parameter :: PBE_B3 =  11.0988_F64
      real(F64), parameter :: PBE_B4 =  -5.11013_F64
      real(F64), parameter :: PBE_B5 =   1.71468_F64
      real(F64), parameter :: PBE_B6 =  -0.610380_F64
      real(F64), parameter :: PBE_B7 =   0.307555_F64
      real(F64), parameter :: PBE_B8 =  -0.0770547_F64
      real(F64), parameter :: PBE_B9 =   0.0334840_F64

      real(F64), parameter :: PBESOL_A2 =  0.0047333_F64
      real(F64), parameter :: PBESOL_A3 =  0.0403304_F64
      real(F64), parameter :: PBESOL_A4 = -0.0574615_F64
      real(F64), parameter :: PBESOL_A5 =  0.0435395_F64
      real(F64), parameter :: PBESOL_A6 = -0.0216251_F64
      real(F64), parameter :: PBESOL_A7 =  0.0063721_F64

      real(F64), parameter :: PBESOL_B1 =  8.52056_F64
      real(F64), parameter :: PBESOL_B2 = -13.9885_F64
      real(F64), parameter :: PBESOL_B3 =  9.28583_F64
      real(F64), parameter :: PBESOL_B4 = -3.27287_F64
      real(F64), parameter :: PBESOL_B5 =  0.843499_F64
      real(F64), parameter :: PBESOL_B6 = -0.235543_F64
      real(F64), parameter :: PBESOL_B7 =  0.0847074_F64
      real(F64), parameter :: PBESOL_B8 = -0.0171561_F64
      real(F64), parameter :: PBESOL_B9 =  0.0050552_F64
      !
      ! Parameters for the B88 exchange defined in Table 1 of Ref. 3
      !
      real(F64), parameter :: B88_A2 =  0.0253933_F64
      real(F64), parameter :: B88_A3 = -0.0673075_F64
      real(F64), parameter :: B88_A4 =  0.0891476_F64
      real(F64), parameter :: B88_A5 = -0.0454168_F64
      real(F64), parameter :: B88_A6 = -0.0076581_F64
      real(F64), parameter :: B88_A7 =  0.0142506_F64

      real(F64), parameter :: B88_B1 = -2.65060_F64
      real(F64), parameter :: B88_B2 =  3.91108_F64
      real(F64), parameter :: B88_B3 = -3.31509_F64
      real(F64), parameter :: B88_B4 =  1.54485_F64
      real(F64), parameter :: B88_B5 = -0.198386_F64
      real(F64), parameter :: B88_B6 = -0.136112_F64
      real(F64), parameter :: B88_B7 =  0.0647862_F64
      real(F64), parameter :: B88_B8 =  0.0159586_F64
      real(F64), parameter :: B88_B9 = -2.45066E-4_F64
      
      real(F64), parameter :: HJS_X_RHO_THRESH = 1.0E-10_F64

contains

      subroutine hjs_sh_pbe_x(eps, ex_rho, ex_sig, rho, sigma, srfrac, omega)
            real(F64), intent(out) :: eps
            real(F64), intent(out) :: ex_rho
            real(F64), intent(out) :: ex_sig
            real(F64), intent(in)  :: rho
            real(F64), intent(in)  :: sigma
            real(F64), intent(in)  :: srfrac
            real(F64), intent(in)  :: omega

            real(F64) :: fr_eps, fr_rho, fr_sig
            real(F64) :: lr_eps, lr_rho, lr_sig
            real(F64) :: sr_eps, sr_rho, sr_sig

            call pbe_x(fr_eps, fr_rho, fr_sig, rho, sigma)
            call hjs_sr_pbe_x(sr_eps, sr_rho, sr_sig, rho, sigma, omega)
            lr_eps = fr_eps - sr_eps
            lr_rho = fr_rho - sr_rho
            lr_sig = fr_sig - sr_sig
            eps = srfrac * sr_eps + lr_eps
            ex_rho = srfrac * sr_rho + lr_rho
            ex_sig = srfrac * sr_sig + lr_sig
      end subroutine hjs_sh_pbe_x
      

      pure subroutine hjs_sr_b88_x(sr_eps, sr_rho, sr_sigma, rho, sigma, omega)
            !
            ! Compute the short-range B88 exchange based on the GGA hole of Ref. 1.
            ! Note that the B88 exchange hole of Ref. 1 is different from Ref. 2.
            ! The improved B88 exchange of Ref. 1 employs different parametrization of H(s)
            ! and the effective reduced gradient.
            !
            ! 1. Weintraub, E., Henderson, T.M., Scuseria, G.E., J. Chem. Theory Comput. 5, 754 (2009);
            !    doi: 10.1021/ct800530u
            ! 2. Henderson, Thomas M., Janesko, Benjamin G., and Scuseria, Gustavo E.,
            !    J. Chem. Phys. 128, 194105 (2008); doi: 10.1063/1.2921797
            !
            real(F64), intent(out) :: sr_eps
            real(F64), intent(out) :: sr_rho
            real(F64), intent(out) :: sr_sigma
            real(F64), intent(in)  :: rho
            real(F64), intent(in)  :: sigma
            real(F64), intent(in)  :: omega
            
            real(F64) :: s, s_rho, s_sigma, kf, kf_rho

            call eff_reduced_grad(s, s_rho, s_sigma, kf, kf_rho, rho, sigma)
            call hjs_x_template(rho, sr_eps, sr_rho, sr_sigma, omega, &
                  B88_A2, B88_A3, B88_A4, B88_A5, B88_A6, B88_A7, &
                  B88_B1, B88_B2, B88_B3, B88_B4, B88_B5, B88_B6, &
                  B88_B7, B88_B8, B88_B9, s, s_rho, s_sigma, kf, kf_rho)
      end subroutine hjs_sr_b88_x

      
      pure subroutine hjs_sr_pbesol_x(sr_eps, sr_rho, sr_sigma, rho, sigma, omega)
            !
            ! Compute the short-range PBEsol exchange based on the GGA hole of Ref. 1.
            !
            ! 1. Henderson, Thomas M., Janesko, Benjamin G., and Scuseria, Gustavo E.,
            !    J. Chem. Phys. 128, 194105 (2008); doi: 10.1063/1.2921797
            !
            real(F64), intent(out) :: sr_eps
            real(F64), intent(out) :: sr_rho
            real(F64), intent(out) :: sr_sigma
            real(F64), intent(in)  :: rho
            real(F64), intent(in)  :: sigma
            real(F64), intent(in)  :: omega

            real(F64) :: s, s_rho, s_sigma, kf, kf_rho

            call reduced_grad(s, s_rho, s_sigma, kf, kf_rho, rho, sigma)
            call hjs_x_template(rho, sr_eps, sr_rho, sr_sigma, omega, &
                  PBESOL_A2, PBESOL_A3, PBESOL_A4, PBESOL_A5, PBESOL_A6, PBESOL_A7, &
                  PBESOL_B1, PBESOL_B2, PBESOL_B3, PBESOL_B4, PBESOL_B5, PBESOL_B6, &
                  PBESOL_B7, PBESOL_B8, PBESOL_B9, s, s_rho, s_sigma, kf, kf_rho)
      end subroutine hjs_sr_pbesol_x


      pure subroutine hjs_sr_pbe_x(sr_eps, sr_rho, sr_sigma, rho, sigma, omega)
            !
            ! Compute the short-range PBE exchange based on the GGA hole of Ref. 1.
            !
            ! 1. Henderson, Thomas M., Janesko, Benjamin G., and Scuseria, Gustavo E.,
            !    J. Chem. Phys. 128, 194105 (2008); doi: 10.1063/1.2921797
            !
            real(F64), intent(out) :: sr_eps
            real(F64), intent(out) :: sr_rho
            real(F64), intent(out) :: sr_sigma
            real(F64), intent(in)  :: rho
            real(F64), intent(in)  :: sigma
            real(F64), intent(in)  :: omega
            
            real(F64) :: s, s_rho, s_sigma, kf, kf_rho

            call reduced_grad(s, s_rho, s_sigma, kf, kf_rho, rho, sigma)
            call hjs_x_template(rho, sr_eps, sr_rho, sr_sigma, omega, &
                  PBE_A2, PBE_A3, PBE_A4, PBE_A5, PBE_A6, PBE_A7, &
                  PBE_B1, PBE_B2, PBE_B3, PBE_B4, PBE_B5, PBE_B6, &
                  PBE_B7, PBE_B8, PBE_B9, s, s_rho, s_sigma, kf, kf_rho)
      end subroutine hjs_sr_pbe_x


      pure subroutine reduced_grad(s, s_rho, s_sigma, kf, kf_rho, rho, sigma)
            !
            ! Compute the reduced density gradient and its derivatives with
            ! respect to rho and sigma. MIN_SIGMA is used if sigma < MIN_SIGMA.
            !
            real(F64), intent(out) :: s
            real(F64), intent(out) :: s_rho
            real(F64), intent(out) :: s_sigma
            real(F64), intent(out) :: kf
            real(F64), intent(out) :: kf_rho
            real(F64), intent(in)  :: rho
            real(F64), intent(in)  :: sigma

            real(F64) :: rho13, sigma12, sig
            real(F64), parameter :: c_kf = (THREE*PI**2)**(ONE/THREE)
            real(F64), parameter :: min_sigma = 1.0E-30_F64
            !
            ! Fermi wave vector
            !
            rho13 = rho**(ONE/THREE)
            kf = c_kf * rho13
            kf_rho = (ONE/THREE) * c_kf / rho13**2
            !
            ! Reduced density gradient
            ! Test if the density gradient is non-zero to avoid division by zero
            !
            sig = max(sigma, min_sigma)
            sigma12 = sqrt(sig)
            s = sigma12 / (TWO * kf * rho)
            s_sigma = one / (FOUR * kf * rho * sigma12)
            s_rho = -s / rho - s / kf * kf_rho
      end subroutine reduced_grad


      pure subroutine eff_reduced_grad(seff, seff_rho, seff_sig, kf, kf_rho, rho, sigma)
            !
            ! Compute the effective reduced density gradient and its derivatives with
            ! respect to rho and sigma. MIN_SIGMA is used if sigma < MIN_SIGMA.
            !
            ! The effective reduced gradient is defined in Eq. Eq. 21 of Ref. 1.
            ! Use the effective gradient only for the short-range B88 exchange.
            !
            ! 1. Weintraub, E., Henderson, T.M., Scuseria, G.E., J. Chem. Theory Comput. 5, 754 (2009);
            !    doi: 10.1021/ct800530u
            !
            real(F64), intent(out) :: seff
            real(F64), intent(out) :: seff_rho
            real(F64), intent(out) :: seff_sig
            real(F64), intent(out) :: kf
            real(F64), intent(out) :: kf_rho
            real(F64), intent(in)  :: rho
            real(F64), intent(in)  :: sigma

            real(F64) :: rho13, sigma12, sig, s, s_sigma, s_rho, exps, seff_s
            real(F64), parameter :: c_kf = (THREE*PI**2)**(ONE/THREE)
            real(F64), parameter :: xi = ONE / (exp(20.0_F64) - ONE)
            real(F64), parameter :: min_sigma = 1.0E-30_F64
            !
            ! Fermi wave vector
            !
            rho13 = rho**(ONE/THREE)
            kf = c_kf * rho13
            kf_rho = (ONE/THREE) * c_kf / rho13**2
            !
            ! Reduced density gradient
            ! Test if the density gradient is non-zero to avoid division by zero
            !
            sig = max(sigma, min_sigma)
            sigma12 = sqrt(sig)
            s = sigma12 / (TWO * kf * rho)
            s_sigma = one / (FOUR * kf * rho * sigma12)
            s_rho = -s / rho - s / kf * kf_rho
            !
            ! Effective gradient
            !
            exps = exp(-s)
            seff = -log((exps + xi) / (ONE + xi))
            seff_s = exps / (exps + xi)
            seff_sig = seff_s * s_sigma
            seff_rho = seff_s * s_rho
      end subroutine eff_reduced_grad

      
      pure subroutine hjs_x_template(rho, eps, vrho, vsigma, omega, &
            a2, a3, a4, a5, a6, a7, b1, b2, b3, b4, b5, b6, b7, b8, b9, &
            s, s_rho, s_sigma, kf, kf_rho)
            !
            ! Template for the short-range PBE, PBEsol, and B88 exchange functionals
            ! based on the GGA exchange hole model of Refs. 1 and 2.
            !
            ! Check if the electron density is non-negligible before calling this
            ! subroutine.
            !
            ! 1. Henderson, Thomas M., Janesko, Benjamin G., and Scuseria, Gustavo E.,
            !    J. Chem. Phys. 128, 194105 (2008); doi: 10.1063/1.2921797
            ! 2. Weintraub, E., Henderson, T.M., Scuseria, G.E., J. Chem. Theory Comput. 5, 754 (2009);
            !    doi: 10.1021/ct800530u
            !
            real(F64), intent(in)  :: rho
            real(F64), intent(out) :: eps
            real(F64), intent(out) :: vrho
            real(F64), intent(out) :: vsigma
            real(F64), intent(in)  :: omega
            real(F64), intent(in)  :: a2, a3, a4, a5, a6, a7
            real(F64), intent(in)  :: b1, b2, b3, b4, b5, b6, b7, b8, b9
            real(F64), intent(in)  :: s, s_rho, s_sigma
            real(F64), intent(in)  :: kf, kf_rho

            real(F64) :: s2, fbar, fbar_s
            real(F64) :: gbar, gbar_lambda, gbar_s
            real(F64) :: hnumer, hdenom, hnumer_s, hdenom_s
            real(F64) :: h, h_s
            real(F64) :: s2h, s2h_s
            real(F64) :: nu, nu2, nu_kf
            real(F64) :: chi, chi_kf, chi_s, chi_nu, chi2, chi_lambda
            real(F64) :: lambda, lambda_s, lambda2, lambda3, lambda12, lambda72
            real(F64) :: term1, term2, term3, term4, term5, term6
            real(F64) :: term1_kf, term1_s, term2_kf, term2_s, term3_kf, term3_s
            real(F64) :: term4_kf, term4_s, term5_kf, term5_s, term6_kf, term6_s
            real(F64) :: t1, t2, t3, t4, t5, t6, t7, t8, t9
            real(F64) :: poly3, poly3_chi, poly5, poly5_chi
            real(F64) :: sqzeta, sqzeta_kf, sqzeta_s
            real(F64) :: sqeta, sqeta_kf, sqeta_s
            real(F64) :: sqlambda, sqlambda_kf, sqlambda_s
            real(F64) :: f1, f1_nu, f1_sqzeta, f1_sqlambda, f1_kf, f1_s, logf1
            real(F64) :: f2, f2_nu, f2_sqeta, f2_sqlambda, f2_kf, f2_s, logf2
            real(F64) :: fxgga, fxgga_kf, fxgga_s
            real(F64) :: zeta, zeta_s, zeta12, zeta12_s
            real(F64) :: eta, eta_s, eta12, eta12_s
            real(F64) :: da2, da3, da4, da5, da6, da7
            real(F64) :: db1, db2, db3, db4, db5
            real(F64) :: db6, db7, db8, db9

            real(F64), parameter :: abar =  0.757211d+0
            real(F64), parameter :: b = -0.106364d+0
            real(F64), parameter :: c = -0.118649d+0
            real(F64), parameter :: d =  0.609650d+0
            real(F64), parameter :: e = -0.0477963d+0

            real(F64), parameter :: c_gbar1 = -2.0_F64/5.0_F64 * c / e
            real(F64), parameter :: c_gbar2 = -4.0_F64/15d+0 * b / e
            real(F64), parameter :: c_gbar3 = -6.0_F64/5.0_F64 * abar / e
            real(F64), parameter :: c_gbar4 = -4.0_F64/5.0_F64 * PI12 / e
            real(F64), parameter :: c_gbar5 = -12.0_F64/5.0_F64 / e

            real(F64), parameter :: c_term1 = -4.0_F64 / 9.0_F64 * b
            real(F64), parameter :: c_term2 = -4.0_F64 / 9.0_F64 * c
            real(F64), parameter :: c_term3 = -8.0_F64 / 9.0_F64 * e
            !
            ! Coeff in front of the exchange integral, Eq. 27 in [1]
            !
            real(F64), parameter :: c_ex = -3.0_F64 / (4.0_F64 * PI)
            !
            ! C_FKBAR0 = 1/s0**2
            !
            real(F64), parameter :: c_fbar0 = 1.0_F64 / 4.0_F64
            real(F64), parameter :: c_fbar1 = -1.0_F64 / (27.0_F64 * c)
            real(F64), parameter :: c_fbar2 = -1.0_F64 / (2.0_F64 * c)

            da2 = 2.0_F64 * a2
            da3 = 3.0_F64 * a3
            da4 = 4.0_F64 * a4
            da5 = 5.0_F64 * a5
            da6 = 6.0_F64 * a6
            da7 = 7.0_F64 * a7

            db1 = b1
            db2 = 2.0_F64 * b2
            db3 = 3.0_F64 * b3
            db4 = 4.0_F64 * b4
            db5 = 5.0_F64 * b5
            db6 = 6.0_F64 * b6
            db7 = 7.0_F64 * b7
            db8 = 8.0_F64 * b8
            db9 = 9.0_F64 * b9

            s2 = s**2
            !
            ! H(s) function, Eq. 42 in [1]
            ! ---
            ! Numerator
            !
            t7 = a6 + s * a7
            t6 = a5 + s * t7
            t5 = a4 + s * t6
            t4 = a3 + s * t5
            t3 = a2 + s * t4
            hnumer = s2 * t3
            !
            ! Numerator: Derivative with respect to s
            !
            t7 = da6 + s * da7
            t6 = da5 + s * t7
            t5 = da4 + s * t6
            t4 = da3 + s * t5
            t3 = da2 + s * t4
            hnumer_s = s * t3
            !
            ! Denominator
            !
            t9 = b8 + s * b9
            t8 = b7 + s * t9
            t7 = b6 + s * t8
            t6 = b5 + s * t7
            t5 = b4 + s * t6
            t4 = b3 + s * t5
            t3 = b2 + s * t4
            t2 = b1 + s * t3
            hdenom = one + s * t2
            !
            ! Denominator: Derivative with respect to s
            !
            t9 = db8 + s * db9
            t8 = db7 + s * t9
            t7 = db6 + s * t8
            t6 = db5 + s * t7
            t5 = db4 + s * t6
            t4 = db3 + s * t5
            t3 = db2 + s * t4
            hdenom_s = db1 + s * t3

            h = hnumer / hdenom
            h_s = hnumer_s / hdenom - hnumer / hdenom**2 * hdenom_s
            !
            ! Fbar function: See Eq. 36 in [1]
            !
            s2h = s2 * h
            s2h_s = TWO * s * h + s2 * h_s
            fbar = one + c_fbar1 * s2 / (one + c_fbar0 * s2) + c_fbar2 * s2h
            fbar_s = TWO * c_fbar1 * s / (one + c_fbar0 * s2)**2 + c_fbar2 * s2h_s
            !
            ! Nu: See def. above Eq. 44 in [1]
            !
            nu = omega / kf
            nu2 = nu**2
            nu_kf = -nu / kf
            !
            ! Eq. 38a in [1]
            !
            zeta = s2h
            zeta12 = sqrt(zeta)
            zeta_s = s2h_s
            zeta12_s = (ONE/TWO) / zeta12 * zeta_s
            !
            ! Eq. 38b in [1]
            !
            eta = abar + s2h
            eta12 = sqrt(eta)
            eta_s = s2h_s
            eta12_s = (ONE/TWO) / eta12 * eta_s
            !
            ! Eq. 39c in [1]
            !
            lambda = d + s2h
            lambda2 = lambda**2
            lambda3 = lambda2 * lambda
            lambda12 = sqrt(lambda)
            lambda72 = lambda3 * lambda12
            lambda_s = s2h_s
            !
            ! Gbar: Eq. 41 in [1]
            !
            t3 = c_gbar3 + c_gbar4 * lambda12 + c_gbar5 * (zeta12 - eta12) * lambda12
            t2 = c_gbar2 + lambda * t3
            t1 = c_gbar1 * fbar + lambda * t2
            gbar = lambda * t1
            t2 = three * c_gbar3 + seven/TWO * (c_gbar4 + c_gbar5 * (zeta12 - eta12)) * lambda12
            t1 = TWO * c_gbar2 + lambda * t2
            gbar_lambda = c_gbar1 * fbar + lambda * t1
            gbar_s = gbar_lambda * lambda_s + c_gbar1 * lambda * fbar_s &
                  + c_gbar5 * lambda72 * (zeta12_s - eta12_s)
            !
            ! Chi: Eq. 44 in [1]
            !
            t1 = lambda + nu2
            t2 = sqrt(lambda + nu2)
            chi = nu / t2
            chi_nu = lambda / (t1 * t2)
            chi_lambda = -(ONE/TWO) * chi / t1
            chi_s = chi_lambda * lambda_s
            chi_kf = chi_nu * nu_kf
            chi2 = chi**2
            !
            ! Enhancement factor: Eq. 43 in [1]
            !
            term1 = abar + c_term1 * (one - chi) / lambda
            term1_kf = -c_term1 * chi_kf / lambda
            term1_s  = -c_term1 * chi_s / lambda - c_term1 * (one - chi) / lambda2 * lambda_s

            poly3 = 1.0_F64 + chi * (-3.0_F64/2.0_F64 + 1.0_F64/2.0_F64 * chi2)
            poly3_chi = -3.0_F64/2.0_F64 + 3.0_F64/2.0_F64 * chi2
            term2 = c_term2 * fbar / lambda2 * poly3
            term2_kf = c_term2 * fbar / lambda2 * poly3_chi * chi_kf
            term2_s = c_term2 * fbar_s / lambda2 * poly3 &
                  - TWO * c_term2 * fbar / lambda3 * lambda_s * poly3 &
                  + c_term2 * fbar / lambda2 * poly3_chi * chi_s

            t1 = 5.0_F64/4.0_F64 - 3.0_F64/8.0_F64 * chi2
            t2 = -15.0_F64/8.0_F64 + chi2 * t1
            poly5 = 1.0_F64 + chi * t2
            t1 = 15.0_F64/4.0_F64 - 15.0_F64/8.0_F64 * chi2
            poly5_chi = -15.0_F64/8.0_F64 + chi2 * t1
            term3 = c_term3 * gbar / lambda3 * poly5
            term3_kf = c_term3 * gbar / lambda3 * poly5_chi * chi_kf
            term3_s = c_term3 * gbar_s / lambda3 * poly5 &
                  - three * term3 / lambda * lambda_s &
                  + c_term3 * gbar / lambda3 * poly5_chi * chi_s

            sqzeta = sqrt(zeta + nu2)
            sqzeta_kf = nu / sqzeta * nu_kf
            sqzeta_s = (ONE/TWO) / sqzeta * zeta_s

            sqeta = sqrt(eta + nu2)
            sqeta_kf = nu / sqeta * nu_kf
            sqeta_s = (ONE/TWO) / sqeta * eta_s

            sqlambda = sqrt(lambda + nu2)
            sqlambda_kf = nu / sqlambda * nu_kf
            sqlambda_s = (ONE/TWO) / sqlambda * lambda_s

            term4 = TWO * nu * (sqzeta - sqeta)
            term4_kf = TWO * nu_kf * (sqzeta - sqeta) &
                  + TWO * nu * (sqzeta_kf - sqeta_kf)
            term4_s = TWO * nu * (sqzeta_s - sqeta_s)

            f1 = (nu + sqzeta) / (nu + sqlambda)
            f1_nu = (sqlambda - sqzeta) / (nu + sqlambda)**2
            f1_sqzeta = one / (nu + sqlambda)
            f1_sqlambda = -(nu + sqzeta) / (nu + sqlambda)**2
            f1_kf = f1_nu * nu_kf + f1_sqzeta * sqzeta_kf + f1_sqlambda * sqlambda_kf
            f1_s = f1_sqzeta * sqzeta_s  + f1_sqlambda * sqlambda_s
            logf1 = log(f1)
            term5 = TWO * zeta * logf1
            term5_kf = TWO * zeta / f1 * f1_kf
            term5_s = TWO * zeta_s * logf1 + TWO * zeta / f1 * f1_s

            f2 = (nu + sqeta) / (nu + sqlambda)
            f2_nu = (sqlambda - sqeta) / (nu + sqlambda)**2
            f2_sqeta = one / (nu + sqlambda)
            f2_sqlambda = -(nu + sqeta) / (nu + sqlambda)**2
            f2_kf = f2_nu * nu_kf + f2_sqeta * sqeta_kf + f2_sqlambda * sqlambda_kf
            f2_s = f2_sqeta * sqeta_s + f2_sqlambda * sqlambda_s
            logf2 = log(f2)
            term6 = -TWO * eta * logf2
            term6_kf = -TWO * eta / f2 * f2_kf
            term6_s = -TWO * eta_s * logf2 - TWO * eta / f2 * f2_s
            !
            ! Gather all contributions to the enhancement factor
            !
            fxgga = term1 + term2 + term3 + term4 + term5 + term6
            fxgga_kf = term1_kf + term2_kf + term3_kf + term4_kf + term5_kf + term6_kf
            fxgga_s = term1_s + term2_s + term3_s + term4_s + term5_s + term6_s
            !
            ! Exchange energy density, Eq. 27 in [1]
            !
            eps = c_ex * kf * fxgga
            t1 = c_ex * rho * kf
            vrho = eps + c_ex * rho * kf_rho * fxgga &
                  + t1 * fxgga_kf * kf_rho &
                  + t1 * fxgga_s * s_rho
            vsigma = t1 * fxgga_s * s_sigma
      end subroutine hjs_x_template
end module hjs_x_energy
