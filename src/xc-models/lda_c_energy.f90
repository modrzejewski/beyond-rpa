module lda_c_energy
      use arithmetic
      use math_constants
      use pbe_c_energy

      implicit none

      real(F64), parameter :: LDA_C_RHO_THRESH = PBE_C_RHO_THRESH
      real(F64), parameter :: LDA_C_FULLPOLAR_THRESH = PBE_C_FULLPOLAR_THRESH
      
contains

      subroutine u_lda_c(epsc, ec_rhoa, ec_rhob, rho_a, rho_b)
            real(F64), intent(out) :: epsc
            real(F64), intent(out) :: ec_rhoa
            real(F64), intent(out) :: ec_rhob
            real(F64), intent(in)  :: rho_a
            real(F64), intent(in)  :: rho_b

            real(F64) :: rho, rs
            real(F64) :: zeta, zeta_rhoa, zeta_rhob, ec_rho, ec_zeta

            epsc = ZERO
            ec_rhoa = ZERO
            ec_rhob = ZERO
            rho = rho_a + rho_b
            if (rho > LDA_C_RHO_THRESH) then
                  rs = (THREE/(FOUR*PI))**(ONE/THREE) / rho**(ONE/THREE)
                  zeta = (rho_a - rho_b) / rho
                  zeta_rhoa = TWO * rho_b / rho**2
                  zeta_rhob = -TWO * rho_a / rho**2
                  call  pw92_c(epsc, ec_rho, ec_zeta, rho, rs, zeta)
                  if (rho_a > LDA_C_FULLPOLAR_THRESH) then
                        ec_rhoa = ec_rho + ec_zeta * zeta_rhoa
                  end if
                  if (rho_b > LDA_C_FULLPOLAR_THRESH) then
                        ec_rhob = ec_rho + ec_zeta * zeta_rhob
                  end if
            end if
      end subroutine u_lda_c


      subroutine lda_c(epsc, ec_rho, rho)
            real(F64), intent(out) :: epsc
            real(F64), intent(out) :: ec_rho
            real(F64), intent(in)  :: rho

            real(F64) :: rs, epsc_rs

            epsc = ZERO
            ec_rho = ZERO
            if (rho > LDA_C_RHO_THRESH) then
                  rs = (THREE/(FOUR*PI))**(ONE/THREE) / rho**(ONE/THREE)
                  call pw92_c_unpolar(epsc, epsc_rs, rs)
                  ec_rho = epsc + rho * epsc_rs * (-ONE/THREE) * rs / rho
            end if
      end subroutine lda_c
end module lda_c_energy
