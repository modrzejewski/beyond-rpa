module pbe_xc_auxiliary
      use arithmetic
      use math_constants
      use pbe_x_energy
      use pbe_c_energy

      implicit none

contains

      subroutine u_pbe_vxc_inf(vxc, rho_a, rho_b, sigaa, sigab, sigbb)
            !
            ! The PBE adiabatic connection integrand at infinite coupling strength,
            ! lim_{lambda->Infinity} Vxc(lambda)
            !
            ! Check for small densities is done inside this subroutine.
            ! The values of Vxc are checked against Table 1 in Seidl, M., Perdew, J.P., Kurth, S.,
            ! Phys. Rev. A 62, 012502; doi: 10.1103/PhysRevA.62.012502
            !
            real(F64), intent(out) :: vxc
            real(F64), intent(in)  :: rho_a
            real(F64), intent(in)  :: rho_b
            real(F64), intent(in)  :: sigaa
            real(F64), intent(in)  :: sigab
            real(F64), intent(in)  :: sigbb

            real(F64) :: epsc, vc_rhoa, vc_rhob, vc_sigaa, vc_sigab, vc_sigbb
            real(F64) :: epsx, epsxa, epsxb, ex_rho, ex_sigma
            
            call pbe_vc_inf(epsc, vc_rhoa, vc_rhob, vc_sigaa, vc_sigab, vc_sigbb, PBE_BETA_EXTRADIGITS, &
                  rho_a, rho_b, sigaa, sigab, sigbb)
            
            epsx = ZERO
            if (TWO*rho_a > PBE_X_RHO_THRESH) then
                  call pbe_x(epsxa, ex_rho, ex_sigma, TWO*rho_a, FOUR*sigaa)
                  epsx = epsx + (rho_a / (rho_a + rho_b)) * epsxa
            end if

            if (TWO*rho_b > PBE_X_RHO_THRESH) then
                  call pbe_x(epsxb, ex_rho, ex_sigma, TWO*rho_b, FOUR*sigbb)
                  epsx = epsx + (rho_b / (rho_a + rho_b)) * epsxb
            end if

            vxc = (rho_a + rho_b) * (epsx + epsc)
      end subroutine u_pbe_vxc_inf


      subroutine pbe_vc_inf(epsc, vc_rhoa, vc_rhob, vc_sigaa, vc_sigab, vc_sigbb, beta, &
            rho_a, rho_b, sigaa, sigab, sigbb)
            !
            ! Compute the adiabatic connection integrand, Vc, in the limit of infinite
            ! interaction strength (lambda->inf), using the PBE expression for the correlation
            ! energy. This subroutine accepts an arbitrary value of the parameter beta.
            !
            ! This subroutine accepts partially/fully polarized densities.
            ! This subroutine is checked with Mathematica.
            !
            ! 1. Seidl, M., Perdew, J.P., Kurth, S., Phys. Rev. A 62, 012502 (2000);
            !    doi: 10.1103/PhysRevA.62.012502
            !
            real(F64), intent(out) :: epsc
            real(F64), intent(out) :: vc_rhoa
            real(F64), intent(out) :: vc_rhob
            real(F64), intent(out) :: vc_sigaa
            real(F64), intent(out) :: vc_sigab
            real(F64), intent(out) :: vc_sigbb
            real(F64), intent(in)  :: beta
            real(F64), intent(in)  :: rho_a
            real(F64), intent(in)  :: rho_b
            real(F64), intent(in)  :: sigaa
            real(F64), intent(in)  :: sigab
            real(F64), intent(in)  :: sigbb

            real(F64) :: sigma, vc_sigma

            sigma = sigaa + TWO * sigab + sigbb
            if ((rho_a+rho_b) > PBE_C_RHO_THRESH .and. rho_a > PBE_C_FULLPOLAR_THRESH &
                  .and. rho_b > PBE_C_FULLPOLAR_THRESH) then
                  call pbe_vc_inf_polar(epsc, vc_rhoa, vc_rhob, vc_sigma, beta, rho_a, rho_b, sigma)
                  vc_sigaa = vc_sigma
                  vc_sigab = vc_sigma * TWO
                  vc_sigbb = vc_sigma
            else if ((rho_a+rho_b) > PBE_C_RHO_THRESH .and. rho_a > PBE_C_FULLPOLAR_THRESH) then
                  call pbe_vc_inf_fullpolar(epsc, vc_rhoa, vc_sigma, beta, rho_a, sigma)
                  vc_rhob = ZERO
                  vc_sigaa = vc_sigma
                  vc_sigab = ZERO
                  vc_sigbb = ZERO
            else if ((rho_a+rho_b) > PBE_C_RHO_THRESH .and. rho_b > PBE_C_FULLPOLAR_THRESH) then
                  call pbe_vc_inf_fullpolar(epsc, vc_rhob, vc_sigma, beta, rho_b, sigma)
                  vc_rhoa = ZERO
                  vc_sigaa = ZERO
                  vc_sigab = ZERO
                  vc_sigbb = vc_sigma
            else
                  epsc = ZERO
                  vc_rhoa = ZERO
                  vc_rhob = ZERO
                  vc_sigaa = ZERO
                  vc_sigab = ZERO
                  vc_sigbb = ZERO
            end if
      end subroutine pbe_vc_inf


      subroutine pbe_vc_inf_polar(epsc, vc_rho_a, vc_rho_b, vc_sigma, beta, rho_a, rho_b, sigma)
            !
            ! Compute the adiabatic connection integrand, Vc, in the limit of infinite
            ! interaction strength (lambda->inf), using the PBE expression for the correlation
            ! energy. This subroutine accepts an arbitrary value of the parameter beta.
            !
            ! Do not use this subroutine for fully spin-polarized systems (rho_b = 0).
            ! This subroutine is checked with Mathematica.
            !
            ! 1. Seidl, M., Perdew, J.P., Kurth, S., Phys. Rev. A 62, 012502 (2000);
            !    doi: 10.1103/PhysRevA.62.012502
            !
            real(F64), intent(out) :: epsc
            real(F64), intent(out) :: vc_rho_a
            real(F64), intent(out) :: vc_rho_b
            real(F64), intent(out) :: vc_sigma
            real(F64), intent(in)  :: beta
            real(F64), intent(in)  :: rho_a
            real(F64), intent(in)  :: rho_b
            real(F64), intent(in)  :: sigma

            real(F64) :: rs, rs_rho, zeta, zeta_rho_a, zeta_rho_b 
            real(F64) :: phi, phi_zeta, b1, b1_zeta, b1_rs
            real(F64) :: epscUEG, vcUEG_rho, vcUEG_zeta, d0, d0_zeta, y1, y2
            real(F64) :: rho, t2, t2_rho, t2_sigma, t2_zeta
            real(F64) :: h1, h1_phi, h1_b1, h1_t2, h1_rho, h1_sigma, h1_zeta
            real(F64) :: vc_rho, vc_zeta
            real(F64) :: rho13, rho73

            rho = rho_a + rho_b
            rho13 = rho**(ONE/THREE)
            rho73 = rho**2 * rho13
            rs = (THREE/(FOUR*PI))**(ONE/THREE) / rho13
            rs_rho = -(ONE/THREE) * rs / rho
            zeta = (rho_a - rho_b) / (rho_a + rho_b)
            zeta_rho_a = TWO * rho_b / (rho_a + rho_b)**2
            zeta_rho_b = -TWO * rho_a / (rho_a + rho_b)**2
            call pw92_vc_inf(epscUEG, vcUEG_rho, vcUEG_zeta, d0, d0_zeta, y1, y2, rho, rs, zeta)
            phi = (y1**2 + y2**2) / TWO
            phi_zeta = (ONE / y1 - ONE / y2) / THREE
            t2 = (PI/THREE)**(ONE/THREE)/16.0_F64 * sigma / (phi**2 * rho73)
            t2_sigma = (PI/THREE)**(ONE/THREE)/16.0_F64 / (phi**2 * rho73)
            t2_zeta = -TWO * t2 / phi * phi_zeta
            t2_rho = -SEVEN/THREE * t2 / rho
            !
            ! Eq. D8 in Ref. 1
            !
            b1 = beta / d0 * phi**3 * rs
            b1_rs = beta / d0 * phi**3
            b1_zeta = -b1 / d0 * d0_zeta + THREE * beta / d0 * phi**2 * rs * phi_zeta
            !
            ! Eq. D11 in Ref. 1
            !
            h1 = beta * phi**3 * t2 * (ONE + b1 * t2) / (ONE + b1 * t2 + (b1 * t2)**2)
            h1_phi = THREE * beta * phi**2 * t2 * (ONE + b1 * t2) / (ONE + b1 * t2 + (b1 * t2)**2)
            h1_t2 = beta * phi**3 * (ONE + TWO * b1 * t2) / (ONE + b1 * t2 + (b1 * t2)**2)**2
            h1_b1 = -b1 * beta * phi**3 * t2**3 * (TWO + b1 * t2) / (ONE + b1 * t2 + (b1 * t2)**2)**2
            h1_rho = h1_t2 * t2_rho + h1_b1 * b1_rs * rs_rho
            h1_zeta = h1_t2 * t2_zeta + h1_b1 * b1_zeta + h1_phi * phi_zeta
            h1_sigma = h1_t2 * t2_sigma
            !
            ! Correlation component of the RHS of Eq. D13 in Ref. 1
            !
            epsc = epscUEG + h1
            vc_rho =  vcUEG_rho + h1 + rho * h1_rho
            vc_sigma = rho * h1_sigma
            vc_zeta = vcUEG_zeta + rho * h1_zeta
            vc_rho_a = vc_rho + vc_zeta * zeta_rho_a
            vc_rho_b = vc_rho + vc_zeta * zeta_rho_b
      end subroutine pbe_vc_inf_polar


      subroutine pbe_vc_inf_fullpolar(epsc, vc_rho_a, vc_sigma, beta, rho_a, sigma)
            !
            ! Compute the adiabatic connection integrand, Vc, in the limit of infinite
            ! interaction strength (lambda->inf), using the PBE expression for the correlation
            ! energy. This subroutine accepts an arbitrary value of the parameter beta.
            !
            ! This variant of the subroutine is for fully spin-polarized systems (rho_b = 0).
            ! This subroutine is checked with Mathematica.
            !
            ! 1. Seidl, M., Perdew, J.P., Kurth, S., Phys. Rev. A 62, 012502 (2000);
            !    doi: 10.1103/PhysRevA.62.012502
            !
            real(F64), intent(out) :: epsc
            real(F64), intent(out) :: vc_rho_a
            real(F64), intent(out) :: vc_sigma
            real(F64), intent(in)  :: beta
            real(F64), intent(in)  :: rho_a
            real(F64), intent(in)  :: sigma

            real(F64) :: rs, rs_rho
            real(F64) :: b1, b1_rs
            real(F64) :: epscUEG, vcUEG_rho, vcUEG_zeta, d0, d0_zeta, y1, y2
            real(F64) :: rho, t2, t2_rho, t2_sigma
            real(F64) :: h1, h1_b1, h1_t2, h1_rho, h1_sigma
            real(F64) :: rho13, rho73
            real(F64), parameter :: phi = ONE/TWO**(ONE/THREE)

            rho = rho_a
            rho13 = rho**(ONE/THREE)
            rho73 = rho**2 * rho13
            rs = (THREE/(FOUR*PI))**(ONE/THREE) / rho13
            rs_rho = -(ONE/THREE) * rs / rho
            call pw92_vc_inf(epscUEG, vcUEG_rho, vcUEG_zeta, d0, d0_zeta, y1, y2, rho, rs, ONE)
            t2 = (PI/THREE)**(ONE/THREE)/16.0_F64 * sigma / (phi**2 * rho73)
            t2_sigma = (PI/THREE)**(ONE/THREE)/16.0_F64 / (phi**2 * rho73)
            t2_rho = -SEVEN/THREE * t2 / rho
            !
            ! Eq. D8 in Ref. 1
            !
            b1 = beta / d0 * phi**3 * rs
            b1_rs = beta / d0 * phi**3
            !
            ! Eq. D11 in Ref. 1
            !
            h1 = beta * phi**3 * t2 * (ONE + b1 * t2) / (ONE + b1 * t2 + (b1 * t2)**2)
            h1_t2 = beta * phi**3 * (ONE + TWO * b1 * t2) / (ONE + b1 * t2 + (b1 * t2)**2)**2
            h1_b1 = -b1 * beta * phi**3 * t2**3 * (TWO + b1 * t2) / (ONE + b1 * t2 + (b1 * t2)**2)**2
            h1_rho = h1_t2 * t2_rho + h1_b1 * b1_rs * rs_rho
            h1_sigma = h1_t2 * t2_sigma
            !
            ! Correlation component of the RHS of Eq. D13 in Ref. 1
            !
            epsc = epscUEG + h1
            vc_rho_a =  vcUEG_rho + h1 + rho * h1_rho
            vc_sigma = rho * h1_sigma
      end subroutine pbe_vc_inf_fullpolar


      pure subroutine pw92_vc_inf(epsc, vc_rho, vc_zeta, d0, d0_zeta, y1, y2, rho, rs, zeta)
            !
            ! Compute the adiabatic connection integrand, Vc, in the limit of
            ! infinite interaction strength (lambda->inf), in the PW92 parametrization
            ! of the homogeneous electron gas.
            !
            ! This subroutine is checked with Mathematica.
            !
            ! 1. Seidl, M., Perdew, J.P., Kurth, S., Phys. Rev. A 62, 012502 (2000);
            !    doi: 10.1103/PhysRevA.62.012502
            ! 2. Perdew, J.P., Wang, Y. Phys. Rev. B 45, 13244 (1992);
            !    doi: 10.1103/PhysRevB.45.13244
            !
            real(F64), intent(out) :: epsc
            real(F64), intent(out) :: vc_rho
            real(F64), intent(out) :: vc_zeta
            real(F64), intent(out) :: d0
            real(F64), intent(out) :: d0_zeta
            real(F64), intent(out) :: y1
            real(F64), intent(out) :: y2
            real(F64), intent(in)  :: rho
            real(F64), intent(in)  :: rs
            real(F64), intent(in)  :: zeta

            real(F64) :: w, w_zeta, rs_rho, aa, bb
            real(F64) :: f, f_zeta
            real(F64) :: dxc, dxc_zeta

            aa = abs(ONE + zeta)
            bb = abs(ONE - zeta)
            y1 = aa**(ONE/THREE)
            y2 = bb**(ONE/THREE)
            w = aa * y1 + bb * y2
            w_zeta = FOUR/THREE * (y1 - y2)
            !
            ! Spin-interpolation function defined in Eq. 9 of Ref. 2
            !
            f = (w - TWO) / (TWO**(FOUR/THREE)-TWO)
            f_zeta = w_zeta / (TWO**(FOUR/THREE)-TWO)
            !
            ! dxc coefficient in the low density expansion exc = -dxc/rs + ...
            ! Eq. 28 in Ref. 2
            !
            dxc = 0.4582_F64/TWO * w + 0.4335_F64 - 0.1310_F64 * f + 0.0262_F64 * f * zeta**4
            dxc_zeta = 0.4582_F64/TWO * w_zeta - 0.1310_F64 * f_zeta + &
                  0.0262_F64 * (f_zeta * zeta**4 + FOUR * f * zeta**3)
            !
            ! d0 coefficient defined in Eq. D6 of Ref. 1
            !
            d0 = dxc - THREE/(EIGHT*PI)*(NINE*PI/FOUR)**(ONE/THREE) * w
            d0_zeta = dxc_zeta - THREE/(EIGHT*PI)*(NINE*PI/FOUR)**(ONE/THREE) * w_zeta
            !
            ! The low-density expansion of the correlation energy (Eq. D3 of Ref. 1)
            !
            epsc = -d0 / rs
            vc_zeta = -rho * d0_zeta / rs
            rs_rho = -(ONE/THREE) * rs / rho
            vc_rho = -rho * epsc / rs * rs_rho - d0 / rs
      end subroutine pw92_vc_inf
end module pbe_xc_auxiliary
