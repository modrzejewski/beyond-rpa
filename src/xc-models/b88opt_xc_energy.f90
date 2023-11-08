module b88opt_xc_energy
      use arithmetic
      use math_constants
      use lyp_c_energy
      use b88opt_x_energy

      implicit none

contains

      subroutine b88opt_lyp_xc(eps, vrho, vsigma, rho, sigma, npt, exx)
            !
            ! Compute the optB88X+LYP exchange-correlation energy (closed shell systems).
            !
            real(F64), dimension(:), intent(out) :: eps
            real(F64), dimension(:), intent(out) :: vrho
            real(F64), dimension(:), intent(out) :: vsigma
            real(F64), dimension(:), intent(in)  :: rho
            real(F64), dimension(:), intent(in)  :: sigma
            integer, intent(in)                  :: npt
            real(F64), intent(in)                :: exx

            integer :: k
            real(F64) :: epsx, ex_rho, ex_sigma, xfrac
            real(F64) :: epsc, ec_rhoa, ec_rhob, ec_sigaa, ec_sigab, ec_sigbb
            real(F64) :: rho_a, rho_b, sigma_aa, sigma_ab, sigma_bb

            xfrac = ONE - exx
            do k = 1, npt
                  if (rho(k) > B88OPT_X_RHO_THRESH) then
                        call b88opt_x(epsx, ex_rho, ex_sigma, rho(k), sigma(k))
                        rho_a = rho(k) / TWO
                        rho_b = rho(k) / TWO
                        sigma_aa = sigma(k) / FOUR
                        sigma_ab = sigma(k) / FOUR
                        sigma_bb = sigma(k) / FOUR
                        call lyp_c(epsc, ec_rhoa, ec_rhob, ec_sigaa, ec_sigab, ec_sigbb, rho_a, rho_b, sigma_aa, sigma_ab, sigma_bb)
                        eps(k) = xfrac * epsx + epsc
                        vrho(k) = xfrac * ex_rho + ec_rhoa
                        vsigma(k) = xfrac * ex_sigma + ec_sigaa * (ONE/TWO) + ec_sigab * (ONE/FOUR)
                  else
                        eps(k) = ZERO
                        vrho(k) = ZERO
                        vsigma(k) = ZERO
                  end if
            end do
      end subroutine b88opt_lyp_xc


      subroutine u_b88opt_lyp_xc(eps, vrho, vsigma, rho, sigma, npt, exx)
            !
            ! Compute the optB88x+LYP exchange-correlation energy (open shell systems).
            !
            real(F64), dimension(:), intent(out)    :: eps
            real(F64), dimension(:, :), intent(out) :: vrho
            real(F64), dimension(:, :), intent(out) :: vsigma
            real(F64), dimension(:, :), intent(in)  :: rho
            real(F64), dimension(:, :), intent(in)  :: sigma
            integer, intent(in)                     :: npt
            real(F64), intent(in)                   :: exx

            integer :: k
            real(F64) :: epsx, e_rho, e_sigma, xfrac
            real(F64) :: epsc, ec_rhoa, ec_rhob, ec_sigaa, ec_sigab, ec_sigbb

            xfrac = ONE - exx
            do k = 1, npt
                  eps(k) = ZERO
                  vrho(1, k) = ZERO
                  vrho(2, k) = ZERO
                  vsigma(1, k) = ZERO
                  vsigma(2, k) = ZERO
                  vsigma(3, k) = ZERO
                  
                  if (TWO*rho(1, k) > B88OPT_X_RHO_THRESH .and. TWO*rho(2, k) > B88OPT_X_RHO_THRESH) then
                        call lyp_c(epsc, ec_rhoa, ec_rhob, ec_sigaa, ec_sigab, ec_sigbb, rho(1, k), rho(2, k), &
                              sigma(1, k), sigma(2, k), sigma(3, k))
                        eps(k) = eps(k) + epsc
                        vrho(1, k) = vrho(1, k) + ec_rhoa
                        vrho(2, k) = vrho(2, k) + ec_rhob
                        vsigma(1, k) = vsigma(1, k) + ec_sigaa
                        vsigma(2, k) = vsigma(2, k) + ec_sigab
                        vsigma(3, k) = vsigma(3, k) + ec_sigbb
                  end if

                  if (TWO*rho(1, k) > B88OPT_X_RHO_THRESH) then
                        call b88opt_x(epsx, e_rho, e_sigma, TWO*rho(1, k), FOUR*sigma(1, k))
                        eps(k) = eps(k) + xfrac * (rho(1, k) / (rho(1, k) + rho(2, k))) * epsx
                        vrho(1, k) = vrho(1, k) + xfrac * e_rho
                        vsigma(1, k) = vsigma(1, k) + TWO * xfrac * e_sigma
                  end if

                  if (TWO*rho(2, k) > B88OPT_X_RHO_THRESH) then
                        call b88opt_x(epsx, e_rho, e_sigma, TWO*rho(2, k), FOUR*sigma(3, k))
                        eps(k) = eps(k) + xfrac * (rho(2, k) / (rho(1, k) + rho(2, k))) * epsx
                        vrho(2, k) = vrho(2, k) + xfrac * e_rho
                        vsigma(3, k) = vsigma(3, k) + TWO * xfrac * e_sigma
                  end if                  
            end do
      end subroutine u_b88opt_lyp_xc
end module b88opt_xc_energy
