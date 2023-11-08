module b88_xc_energy
      use arithmetic
      use math_constants
      use b88_x_energy
      use lyp_c_energy

      implicit none

contains

      subroutine b88_xonly(eps, vrho, vsigma, rho, sigma, npt, exx)
            real(F64), dimension(:), intent(out) :: eps
            real(F64), dimension(:), intent(out) :: vrho
            real(F64), dimension(:), intent(out) :: vsigma
            real(F64), dimension(:), intent(in)  :: rho
            real(F64), dimension(:), intent(in)  :: sigma
            integer, intent(in)                  :: npt
            real(F64), intent(in)                :: exx

            integer :: k
            real(F64) :: epsx, e_rho, e_sigma, xfrac

            xfrac = ONE - exx
            do k = 1, npt
                  if (rho(k) > B88_X_RHO_THRESH) then
                        call b88_x(epsx, e_rho, e_sigma, rho(k), sigma(k))
                        eps(k) = xfrac * epsx
                        vrho(k) = xfrac * e_rho
                        vsigma(k) = xfrac * e_sigma
                  else
                        eps(k) = ZERO
                        vrho(k) = ZERO
                        vsigma(k) = ZERO
                  end if
            end do
      end subroutine b88_xonly


      subroutine blyp_xc(eps, vrho, vsigma, rho, sigma, npt, exx)
            !
            ! Compute the BLYP exchange-correlation energy (closed shell systems).
            ! Code checked against NWChem 6.5.
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
                  if (rho(k) > B88_X_RHO_THRESH) then
                        call b88_x(epsx, ex_rho, ex_sigma, rho(k), sigma(k))
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
      end subroutine blyp_xc


      subroutine u_b88_xonly(eps, vrho, vsigma, rho, sigma, npt, exx)
            real(F64), dimension(:), intent(out)    :: eps
            real(F64), dimension(:, :), intent(out) :: vrho
            real(F64), dimension(:, :), intent(out) :: vsigma
            real(F64), dimension(:, :), intent(in)  :: rho
            real(F64), dimension(:, :), intent(in)  :: sigma
            integer, intent(in)                     :: npt
            real(F64), intent(in)                   :: exx

            integer :: k
            real(F64) :: epsx, e_rho, e_sigma, xfrac

            xfrac = ONE - exx
            do k = 1, npt
                  eps(k) = ZERO
                  vrho(1, k) = ZERO
                  vrho(2, k) = ZERO
                  vsigma(1, k) = ZERO
                  vsigma(2, k) = ZERO
                  vsigma(3, k) = ZERO

                  if (TWO*rho(1, k) > B88_X_RHO_THRESH) then
                        call b88_x(epsx, e_rho, e_sigma, TWO*rho(1, k), FOUR*sigma(1, k))
                        eps(k) = eps(k) + xfrac * (rho(1, k) / (rho(1, k) + rho(2, k))) * epsx
                        vrho(1, k) = vrho(1, k) + xfrac * e_rho
                        vsigma(1, k) = vsigma(1, k) + TWO * xfrac * e_sigma
                  end if

                  if (TWO*rho(2, k) > B88_X_RHO_THRESH) then
                        call b88_x(epsx, e_rho, e_sigma, TWO*rho(2, k), FOUR*sigma(3, k))
                        eps(k) = eps(k) + xfrac * (rho(2, k) / (rho(1, k) + rho(2, k))) * epsx
                        vrho(2, k) = vrho(2, k) + xfrac * e_rho
                        vsigma(3, k) = vsigma(3, k) + TWO * xfrac * e_sigma
                  end if                  
            end do
      end subroutine u_b88_xonly


      subroutine u_blyp_xc(eps, vrho, vsigma, rho, sigma, npt, exx)
            !
            ! Compute the BLYP exchange-correlation energy (open shell systems).
            ! Code checked against NWChem 6.5.
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
                  
                  if (TWO*rho(1, k) > B88_X_RHO_THRESH .and. TWO*rho(2, k) > B88_X_RHO_THRESH) then
                        call lyp_c(epsc, ec_rhoa, ec_rhob, ec_sigaa, ec_sigab, ec_sigbb, rho(1, k), rho(2, k), &
                              sigma(1, k), sigma(2, k), sigma(3, k))
                        eps(k) = eps(k) + epsc
                        vrho(1, k) = vrho(1, k) + ec_rhoa
                        vrho(2, k) = vrho(2, k) + ec_rhob
                        vsigma(1, k) = vsigma(1, k) + ec_sigaa
                        vsigma(2, k) = vsigma(2, k) + ec_sigab
                        vsigma(3, k) = vsigma(3, k) + ec_sigbb
                  end if

                  if (TWO*rho(1, k) > B88_X_RHO_THRESH) then
                        call b88_x(epsx, e_rho, e_sigma, TWO*rho(1, k), FOUR*sigma(1, k))
                        eps(k) = eps(k) + xfrac * (rho(1, k) / (rho(1, k) + rho(2, k))) * epsx
                        vrho(1, k) = vrho(1, k) + xfrac * e_rho
                        vsigma(1, k) = vsigma(1, k) + TWO * xfrac * e_sigma
                  end if

                  if (TWO*rho(2, k) > B88_X_RHO_THRESH) then
                        call b88_x(epsx, e_rho, e_sigma, TWO*rho(2, k), FOUR*sigma(3, k))
                        eps(k) = eps(k) + xfrac * (rho(2, k) / (rho(1, k) + rho(2, k))) * epsx
                        vrho(2, k) = vrho(2, k) + xfrac * e_rho
                        vsigma(3, k) = vsigma(3, k) + TWO * xfrac * e_sigma
                  end if                  
            end do
      end subroutine u_blyp_xc


      subroutine ec_blyp_xc(eps, vrho, vsigma, vlapl, vtau, rho, sigma, lapl, tau, npt, srexx, omega)
            real(F64), dimension(:), intent(out) :: eps
            real(F64), dimension(:), intent(out) :: vrho
            real(F64), dimension(:), intent(out) :: vsigma
            real(F64), dimension(:), intent(out) :: vlapl
            real(F64), dimension(:), intent(out) :: vtau
            real(F64), dimension(:), intent(in)  :: rho
            real(F64), dimension(:), intent(in)  :: sigma
            real(F64), dimension(:), intent(in)  :: lapl
            real(F64), dimension(:), intent(in)  :: tau
            integer, intent(in)                  :: npt
            real(F64), intent(in)                :: srexx
            real(F64), intent(in)                :: omega

            integer :: k
            real(F64) :: sr_eps, sr_rho, sr_sigma, sr_lapl, sr_tau, xfrac
            real(F64) :: epsc, ec_rhoa, ec_rhob, ec_sigaa, ec_sigab, ec_sigbb
            real(F64) :: rho_a, rho_b, sigma_aa, sigma_ab, sigma_bb

            xfrac = ONE - srexx
            do k = 1, npt
                  if (rho(k) > B88_X_RHO_THRESH) then
                        call ec_sr_b88_x(sr_eps, sr_rho, sr_sigma, sr_lapl, sr_tau, rho(k), sigma(k), lapl(k), tau(k), omega)
                        rho_a = rho(k) / TWO
                        rho_b = rho(k) / TWO
                        sigma_aa = sigma(k) / FOUR
                        sigma_ab = sigma(k) / FOUR
                        sigma_bb = sigma(k) / FOUR
                        call lyp_c(epsc, ec_rhoa, ec_rhob, ec_sigaa, ec_sigab, ec_sigbb, rho_a, rho_b, sigma_aa, sigma_ab, sigma_bb)
                        eps(k) = xfrac * sr_eps + epsc
                        vrho(k) = xfrac * sr_rho + ec_rhoa
                        vsigma(k) = xfrac * sr_sigma + ec_sigaa * (ONE/TWO) + ec_sigab * (ONE/FOUR)
                        vlapl(k) = xfrac * sr_lapl
                        vtau(k) = xfrac * sr_tau
                  else
                        eps(k) = ZERO
                        vrho(k) = ZERO
                        vsigma(k) = ZERO
                        vlapl(k) = ZERO
                        vtau(k) = ZERO
                  end if
            end do
      end subroutine ec_blyp_xc


      subroutine ec_b88_xonly(eps, vrho, vsigma, vlapl, vtau, rho, sigma, lapl, tau, npt, srexx, omega)
            real(F64), dimension(:), intent(out) :: eps
            real(F64), dimension(:), intent(out) :: vrho
            real(F64), dimension(:), intent(out) :: vsigma
            real(F64), dimension(:), intent(out) :: vlapl
            real(F64), dimension(:), intent(out) :: vtau
            real(F64), dimension(:), intent(in)  :: rho
            real(F64), dimension(:), intent(in)  :: sigma
            real(F64), dimension(:), intent(in)  :: lapl
            real(F64), dimension(:), intent(in)  :: tau
            integer, intent(in)                  :: npt
            real(F64), intent(in)                :: srexx
            real(F64), intent(in)                :: omega

            integer :: k
            real(F64) :: sr_eps, sr_rho, sr_sigma, sr_lapl, sr_tau, xfrac

            xfrac = ONE - srexx
            do k = 1, npt
                  if (rho(k) > B88_X_RHO_THRESH) then
                        call ec_sr_b88_x(sr_eps, sr_rho, sr_sigma, sr_lapl, sr_tau, rho(k), sigma(k), lapl(k), tau(k), omega)
                        eps(k) = xfrac * sr_eps
                        vrho(k) = xfrac * sr_rho
                        vsigma(k) = xfrac * sr_sigma
                        vlapl(k) = xfrac * sr_lapl
                        vtau(k) = xfrac * sr_tau
                  else
                        eps(k) = ZERO
                        vrho(k) = ZERO
                        vsigma(k) = ZERO
                        vlapl(k) = ZERO
                        vtau(k) = ZERO
                  end if
            end do
      end subroutine ec_b88_xonly


      subroutine u_ec_blyp_xc(eps, vrho, vsigma, vlapl, vtau, rho, sigma, lapl, tau, npt, srexx, omega)
            real(F64), dimension(:), intent(out)    :: eps
            real(F64), dimension(:, :), intent(out) :: vrho
            real(F64), dimension(:, :), intent(out) :: vsigma
            real(F64), dimension(:, :), intent(out) :: vlapl
            real(F64), dimension(:, :), intent(out) :: vtau
            real(F64), dimension(:, :), intent(in)  :: rho
            real(F64), dimension(:, :), intent(in)  :: sigma
            real(F64), dimension(:, :), intent(in)  :: lapl
            real(F64), dimension(:, :), intent(in)  :: tau
            integer, intent(in)                     :: npt
            real(F64), intent(in)                   :: srexx
            real(F64), intent(in)                   :: omega

            integer :: k
            real(F64) :: sr_eps, sr_rho, sr_sigma, sr_lapl, sr_tau, xfrac
            real(F64) :: epsc, ec_rhoa, ec_rhob, ec_sigaa, ec_sigab, ec_sigbb

            xfrac = ONE - srexx
            do k = 1, npt
                  eps(k) = ZERO
                  vrho(1, k) = ZERO
                  vrho(2, k) = ZERO
                  vsigma(1, k) = ZERO
                  vsigma(2, k) = ZERO
                  vsigma(3, k) = ZERO
                  vlapl(1, k) = ZERO
                  vlapl(2, k) = ZERO
                  vtau(1, k) = ZERO
                  vtau(2, k) = ZERO
                  
                  if (TWO*rho(1, k) > B88_X_RHO_THRESH .and. TWO*rho(2, k) > B88_X_RHO_THRESH) then
                        call lyp_c(epsc, ec_rhoa, ec_rhob, ec_sigaa, ec_sigab, ec_sigbb, rho(1, k), rho(2, k), &
                              sigma(1, k), sigma(2, k), sigma(3, k))
                        eps(k) = eps(k) + epsc
                        vrho(1, k) = vrho(1, k) + ec_rhoa
                        vrho(2, k) = vrho(2, k) + ec_rhob
                        vsigma(1, k) = vsigma(1, k) + ec_sigaa
                        vsigma(2, k) = vsigma(2, k) + ec_sigab
                        vsigma(3, k) = vsigma(3, k) + ec_sigbb
                  end if

                  if (TWO*rho(1, k) > B88_X_RHO_THRESH) then
                        call ec_sr_b88_x(sr_eps, sr_rho, sr_sigma, sr_lapl, sr_tau, TWO*rho(1, k), FOUR*sigma(1, k), &
                              TWO*lapl(1, k), TWO*tau(1, k), omega)
                        eps(k) = eps(k) + xfrac * (rho(1, k) / (rho(1, k) + rho(2, k))) * sr_eps
                        vrho(1, k) = vrho(1, k) + xfrac * sr_rho
                        vsigma(1, k) = vsigma(1, k) + TWO * xfrac * sr_sigma
                        vlapl(1, k) = vlapl(1, k) + xfrac * sr_lapl
                        vtau(1, k) = vtau(1, k) + xfrac * sr_tau
                  end if
                  
                  if (TWO*rho(2, k) > B88_X_RHO_THRESH) then
                        call ec_sr_b88_x(sr_eps, sr_rho, sr_sigma, sr_lapl, sr_tau, TWO*rho(2, k), FOUR*sigma(3, k), &
                              TWO*lapl(2, k), TWO*tau(2, k), omega)
                        eps(k) = eps(k) + xfrac * (rho(2, k) / (rho(1, k) + rho(2, k))) * sr_eps
                        vrho(2, k) = vrho(2, k) + xfrac * sr_rho
                        vsigma(3, k) = vsigma(3, k) + TWO * xfrac * sr_sigma
                        vlapl(2, k) = vlapl(2, k) + xfrac * sr_lapl
                        vtau(2, k) = vtau(2, k) + xfrac * sr_tau
                  end if
            end do
      end subroutine u_ec_blyp_xc


      subroutine u_ec_b88_xonly(eps, vrho, vsigma, vlapl, vtau, rho, sigma, lapl, tau, npt, srexx, omega)
            real(F64), dimension(:), intent(out)    :: eps
            real(F64), dimension(:, :), intent(out) :: vrho
            real(F64), dimension(:, :), intent(out) :: vsigma
            real(F64), dimension(:, :), intent(out) :: vlapl
            real(F64), dimension(:, :), intent(out) :: vtau
            real(F64), dimension(:, :), intent(in)  :: rho
            real(F64), dimension(:, :), intent(in)  :: sigma
            real(F64), dimension(:, :), intent(in)  :: lapl
            real(F64), dimension(:, :), intent(in)  :: tau
            integer, intent(in)                     :: npt
            real(F64), intent(in)                   :: srexx
            real(F64), intent(in)                   :: omega

            integer :: k
            real(F64) :: sr_eps, sr_rho, sr_sigma, sr_lapl, sr_tau, xfrac

            xfrac = ONE - srexx
            do k = 1, npt
                  eps(k) = ZERO
                  vrho(1, k) = ZERO
                  vrho(2, k) = ZERO
                  vsigma(1, k) = ZERO
                  vsigma(2, k) = ZERO
                  vsigma(3, k) = ZERO
                  vlapl(1, k) = ZERO
                  vlapl(2, k) = ZERO
                  vtau(1, k) = ZERO
                  vtau(2, k) = ZERO
                  
                  if (TWO*rho(1, k) > B88_X_RHO_THRESH) then
                        call ec_sr_b88_x(sr_eps, sr_rho, sr_sigma, sr_lapl, sr_tau, TWO*rho(1, k), FOUR*sigma(1, k), &
                              TWO*lapl(1, k), TWO*tau(1, k), omega)
                        eps(k) = eps(k) + xfrac * (rho(1, k) / (rho(1, k) + rho(2, k))) * sr_eps
                        vrho(1, k) = vrho(1, k) + xfrac * sr_rho
                        vsigma(1, k) = vsigma(1, k) + TWO * xfrac * sr_sigma
                        vlapl(1, k) = vlapl(1, k) + xfrac * sr_lapl
                        vtau(1, k) = vtau(1, k) + xfrac * sr_tau
                  end if
                  
                  if (TWO*rho(2, k) > B88_X_RHO_THRESH) then
                        call ec_sr_b88_x(sr_eps, sr_rho, sr_sigma, sr_lapl, sr_tau, TWO*rho(2, k), FOUR*sigma(3, k), &
                              TWO*lapl(2, k), TWO*tau(2, k), omega)
                        eps(k) = eps(k) + xfrac * (rho(2, k) / (rho(1, k) + rho(2, k))) * sr_eps
                        vrho(2, k) = vrho(2, k) + xfrac * sr_rho
                        vsigma(3, k) = vsigma(3, k) + TWO * xfrac * sr_sigma
                        vlapl(2, k) = vlapl(2, k) + xfrac * sr_lapl
                        vtau(2, k) = vtau(2, k) + xfrac * sr_tau
                  end if
            end do
      end subroutine u_ec_b88_xonly
end module b88_xc_energy
