module lyp_c_energy
      use arithmetic
      use math_constants

      implicit none

contains

      pure subroutine lyp_c(epsc, ec_rhoa, ec_rhob, ec_sigaa, ec_sigab, ec_sigbb, rho_a, rho_b, sigma_aa, sigma_ab, sigma_bb)
            !
            ! Compute the Lee-Yang-Parr correlation energy [1] and its derivatives expressed
            ! by Miehlich et al. [2] as functionals of the density and density gradient.
            !
            ! This code was checked against Mathematica.
            ! Call this subroutine only if both alpha and beta spin densities are non-negligible.
            ! This subroutine accepts density gradients which are exactly zero.
            !
            ! 1. Lee, C., Yang, W., Parr, R.G., Phys. Rev. B 37, 785 (1988); doi: 10.1103/PhysRevB.37.785
            ! 2. Miehlich, B., Savin, A., Stoll, H., Heinzwerner, P., Chem. Phys. Lett. 157, 200 (1989);
            !    doi: 10.1016/0009-2614(89)87234-3
            !
            real(F64), intent(out) :: epsc
            real(F64), intent(out) :: ec_rhoa
            real(F64), intent(out) :: ec_rhob
            real(F64), intent(out) :: ec_sigaa
            real(F64), intent(out) :: ec_sigab
            real(F64), intent(out) :: ec_sigbb
            real(F64), intent(in)  :: rho_a
            real(F64), intent(in)  :: rho_b
            real(F64), intent(in)  :: sigma_aa
            real(F64), intent(in)  :: sigma_ab
            real(F64), intent(in)  :: sigma_bb

            real(F64) :: rhom13, rhom13_rho, rhom43, rhom113, rho, sigma
            real(F64) :: rhoa83, rhob83, omega, omega_rho, delta, delta_rho
            real(F64) :: s1, s1_rho, s2, s0, s0_rho, s0_rhoa, s0_rhob
            real(F64) :: t1, t1_rhoa, t1_rhob, t2, t2_rho, t2_sigma, t3, t3_rho
            real(F64) :: t3_sigaa, t3_sigbb, t4, t4_rho, t4_rhoa, t4_rhob, t4_sigaa
            real(F64) :: t4_sigbb, w1, w1_rho, w1_rhoa, w1_rhob, w1_sigma, w1_sigaa, w1_sigbb
            real(F64) :: w0, w0_rho, w0_rhoa, w0_rhob, w0_sigma, w0_sigaa, w0_sigbb
            real(F64) :: w2, w2_rho, w2_sigma, w3, w3_rho, w3_rhoa, w3_sigbb
            real(F64) :: w4, w4_rho, w4_rhob, w4_sigaa
            real(F64), parameter :: a = 0.04918_F64
            real(F64), parameter :: b = 0.132_F64
            real(F64), parameter :: c = 0.2533_F64
            real(F64), parameter :: d = 0.349_F64
            real(F64), parameter :: CF = THREE/TEN*(THREE*PI**2)**(TWO/THREE)

            rho = rho_a + rho_b
            sigma = sigma_aa + TWO * sigma_ab + sigma_bb
            rhom13 = ONE/rho**(ONE/THREE)
            rhom13_rho = -(ONE/THREE) * rhom13 / rho
            rhom43 = rhom13 / rho
            rhom113 = rhom43**2 / rho
            !
            ! Compute the intermediates required to assemble the right-hand side of Eq. 2 in Ref. 2
            !
            omega = exp(-c * rhom13) / (ONE + d * rhom13) * rhom113
            omega_rho = -c * rhom13_rho * omega - omega / (ONE + d * rhom13) * d * rhom13_rho &
                  - (ELEVEN/THREE) * omega / rho
            delta = c * rhom13 + d * rhom13 / (ONE + d * rhom13)
            delta_rho = c * rhom13_rho + d * rhom13_rho / (ONE + d * rhom13) &
                  - d**2 * rhom13 / (ONE + d * rhom13)**2 * rhom13_rho

            s1 = FOUR / (ONE + d * rhom13)
            s1_rho = -FOUR / (ONE + d * rhom13)**2 * d * rhom13_rho
            s2 = rho_a * rho_b / rho
            s0 = -a * s1 * s2
            s0_rho = -a * (s1_rho * s2 - s1 * s2 / rho)
            s0_rhoa = -a * s1 * rho_b / rho
            s0_rhob = -a * s1 * rho_a / rho

            rhoa83 = rho_a**(EIGHT/THREE)
            rhob83 = rho_b**(EIGHT/THREE)
            t1 = TWO**(ELEVEN/THREE)*CF * (rhoa83 + rhob83)
            t1_rhoa = (EIGHT/THREE)*TWO**(ELEVEN/THREE)*CF * rhoa83 / rho_a
            t1_rhob = (EIGHT/THREE)*TWO**(ELEVEN/THREE)*CF * rhob83 / rho_b
            
            t2 = (47.0_F64/18.0_F64 - 7.0_F64/18.0_F64 * delta) * sigma
            t2_rho = -7.0_F64/18.0_F64 * delta_rho * sigma
            t2_sigma = 47.0_F64/18.0_F64 - 7.0_F64/18.0_F64 * delta

            t3 = -(5.0_F64/2.0_F64 - 1.0_F64/18.0_F64 * delta) * (sigma_aa + sigma_bb)
            t3_rho = 1.0_F64/18.0_F64 * delta_rho * (sigma_aa + sigma_bb)
            t3_sigaa = -(5.0_F64/2.0_F64 - 1.0_F64/18.0_F64 * delta)
            t3_sigbb = -(5.0_F64/2.0_F64 - 1.0_F64/18.0_F64 * delta)

            t4 = -(delta - ELEVEN) / NINE * (rho_a * sigma_aa + rho_b * sigma_bb) / rho
            t4_rho = -delta_rho / NINE * (rho_a * sigma_aa + rho_b * sigma_bb) / rho - t4 / rho
            t4_rhoa = -(delta - ELEVEN) / NINE * sigma_aa / rho
            t4_rhob = -(delta - ELEVEN) / NINE * sigma_bb / rho
            t4_sigaa = -(delta - ELEVEN) / NINE * rho_a / rho
            t4_sigbb = -(delta - ELEVEN) / NINE * rho_b / rho

            w1 = rho_a * rho_b * (t1 + t2 + t3 + t4)
            w1_rho = rho_a * rho_b * (t2_rho + t3_rho + t4_rho)
            w1_rhoa = rho_b * (t1 + t2 + t3 + t4) + rho_a * rho_b * (t1_rhoa + t4_rhoa)
            w1_rhob = rho_a * (t1 + t2 + t3 + t4) + rho_a * rho_b * (t1_rhob + t4_rhob)
            w1_sigma = rho_a * rho_b * t2_sigma
            w1_sigaa = rho_a * rho_b * (t3_sigaa + t4_sigaa)
            w1_sigbb = rho_a * rho_b * (t3_sigbb + t4_sigbb)

            w2 = -TWO/THREE * rho**2 * sigma
            w2_rho = -FOUR/THREE * rho * sigma
            w2_sigma = -TWO/THREE * rho**2

            w3 = (TWO/THREE * rho**2 - rho_a**2) * sigma_bb
            w3_rho = FOUR/THREE * rho * sigma_bb
            w3_rhoa = -TWO * rho_a * sigma_bb
            w3_sigbb = TWO/THREE * rho**2 - rho_a**2

            w4 = (TWO/THREE * rho**2 - rho_b**2) * sigma_aa
            w4_rho = FOUR/THREE * rho * sigma_aa
            w4_rhob = -TWO * rho_b * sigma_aa
            w4_sigaa = TWO/THREE * rho**2 - rho_b**2

            w0 = -a*b * omega * (w1 + w2 + w3 + w4)
            w0_rho = -a*b * omega_rho * (w1 + w2 + w3 + w4) - a*b * omega * (w1_rho + w2_rho + w3_rho + w4_rho)
            w0_rhoa = -a*b * omega * (w1_rhoa + w3_rhoa)
            w0_rhob = -a*b * omega * (w1_rhob + w4_rhob)
            w0_sigma = -a*b * omega * (w1_sigma + w2_sigma)
            w0_sigaa = -a*b * omega * (w1_sigaa + w4_sigaa)
            w0_sigbb = -a*b * omega * (w1_sigbb + w3_sigbb)
            !
            ! Final energy and derivatives (Eq. 2 in Ref. 2)
            !
            epsc = (s0 + w0) / rho
            ec_rhoa = s0_rho + s0_rhoa + w0_rho + w0_rhoa
            ec_rhob = s0_rho + s0_rhob + w0_rho + w0_rhob
            ec_sigaa = w0_sigma + w0_sigaa
            ec_sigab = w0_sigma * TWO
            ec_sigbb = w0_sigma + w0_sigbb
      end subroutine lyp_c
end module lyp_c_energy
