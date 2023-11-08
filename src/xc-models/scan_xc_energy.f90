module scan_xc_energy
      use math_constants
      use arithmetic
      use scan_c_energy
      use scan_x_energy
      
      implicit none

contains
      
      subroutine scan_xc(eps, vrho, vsigma, vtau, rho, sigma, tau, npt, exx)
            real(F64), dimension(:), intent(out) :: eps
            real(F64), dimension(:), intent(out) :: vrho
            real(F64), dimension(:), intent(out) :: vsigma
            real(F64), dimension(:), intent(out) :: vtau
            real(F64), dimension(:), intent(in)  :: rho
            real(F64), dimension(:), intent(in)  :: sigma
            real(F64), dimension(:), intent(in)  :: tau
            integer, intent(in)                  :: npt
            real(F64), intent(in)                :: exx

            integer :: k
            real(F64) :: epsx, ex_rho, ex_sigma, ex_tau, xfrac
            real(F64) :: epsc, ec_rho, ec_sigma, ec_tau

            xfrac = ONE - exx
            do k = 1, npt
                  call scan_c(epsc, ec_rho, ec_sigma, ec_tau, rho(k), sigma(k), tau(k))
                  if (rho(k) > SCAN_X_RHO_THRESH) then
                        call scan_x(epsx, ex_rho, ex_sigma, ex_tau, rho(k), sigma(k), tau(k))
                  else
                        epsx = ZERO
                        ex_rho = ZERO
                        ex_sigma = ZERO
                        ex_tau = ZERO
                  end if
                  eps(k) = epsc + xfrac * epsx
                  vrho(k) = ec_rho + xfrac * ex_rho
                  vsigma(k) = ec_sigma + xfrac * ex_sigma
                  vtau(k) = ec_tau + xfrac * ex_tau
            end do
      end subroutine scan_xc


      subroutine u_scan_xc(eps, vrho, vsigma, vtau, rho, sigma, tau, npt, exx)
            real(F64), dimension(:), intent(out)    :: eps
            real(F64), dimension(:, :), intent(out) :: vrho
            real(F64), dimension(:, :), intent(out) :: vsigma
            real(F64), dimension(:, :), intent(out) :: vtau
            real(F64), dimension(:, :), intent(in)  :: rho
            real(F64), dimension(:, :), intent(in)  :: sigma
            real(F64), dimension(:, :), intent(in)  :: tau
            integer, intent(in)                     :: npt
            real(F64), intent(in)                   :: exx

            integer :: k
            real(F64) :: epsc, ec_rhoa, ec_rhob, ec_sigaa, ec_sigab, ec_sigbb, ec_taua, ec_taub
            real(F64) :: epsx, ex_rho, ex_sigma, ex_tau, xfrac

            xfrac = ONE - exx
            do k = 1, npt
                  call u_scan_c(epsc, ec_rhoa, ec_rhob, ec_sigaa, ec_sigab, ec_sigbb, ec_taua, ec_taub, &
                        rho(1, k), rho(2, k), sigma(1, k), sigma(2, k), sigma(3, k), tau(1, k), tau(2, k))
                  eps(k) = epsc
                  vrho(1, k) = ec_rhoa
                  vrho(2, k) = ec_rhob
                  vsigma(1, k) = ec_sigaa
                  vsigma(2, k) = ec_sigab
                  vsigma(3, k) = ec_sigbb
                  vtau(1, k) = ec_taua
                  vtau(2, k) = ec_taub

                  if (TWO*rho(1, k) > SCAN_X_RHO_THRESH) then
                        call scan_x(epsx, ex_rho, ex_sigma, ex_tau, TWO*rho(1, k), FOUR*sigma(1, k), TWO*tau(1, k))
                        eps(k) = eps(k) + xfrac * (rho(1, k) / (rho(1, k) + rho(2, k))) * epsx
                        vrho(1, k) = vrho(1, k) + xfrac * ex_rho
                        vsigma(1, k) = vsigma(1, k) + TWO * xfrac * ex_sigma
                        vtau(1, k) = vtau(1, k) + xfrac * ex_tau
                  end if
                  
                  if (TWO*rho(2, k) > SCAN_X_RHO_THRESH) then
                        call scan_x(epsx, ex_rho, ex_sigma, ex_tau, TWO*rho(2, k), FOUR*sigma(3, k), TWO*tau(2, k))
                        eps(k) = eps(k) + xfrac * (rho(2, k) / (rho(1, k) + rho(2, k))) * epsx
                        vrho(2, k) = vrho(2, k) + xfrac * ex_rho
                        vsigma(3, k) = vsigma(3, k) + TWO * xfrac * ex_sigma
                        vtau(2, k) = vtau(2, k) + xfrac * ex_tau
                  end if
            end do
      end subroutine u_scan_xc


      subroutine ec_scan_xc(eps, vrho, vsigma, vlapl, vtau, rho, sigma, lapl, tau, npt, srexx, omega)
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
            real(F64) :: epsc, ec_rho, ec_sigma, ec_tau

            xfrac = ONE - srexx
            do k = 1, npt
                  call scan_c(epsc, ec_rho, ec_sigma, ec_tau, rho(k), sigma(k), tau(k))
                  if (rho(k) > SCAN_X_RHO_THRESH) then
                        call ec_sr_scan_x(sr_eps, sr_rho, sr_sigma, sr_lapl, sr_tau, rho(k), sigma(k), lapl(k), tau(k), omega)
                  else
                        sr_eps = ZERO
                        sr_rho = ZERO
                        sr_sigma = ZERO
                        sr_lapl = ZERO
                        sr_tau = ZERO
                  end if
                  eps(k) = epsc + xfrac * sr_eps
                  vrho(k) = ec_rho + xfrac * sr_rho
                  vsigma(k) = ec_sigma + xfrac * sr_sigma
                  vlapl(k) = xfrac * sr_lapl
                  vtau(k) = ec_tau + xfrac * sr_tau
            end do
      end subroutine ec_scan_xc


      subroutine u_ec_scan_xc(eps, vrho, vsigma, vlapl, vtau, rho, sigma, lapl, tau, npt, srexx, omega)
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
            real(F64) :: epsc, ec_rhoa, ec_rhob, ec_sigaa, ec_sigab, ec_sigbb, ec_taua, ec_taub

            xfrac = ONE - srexx
            do k = 1, npt
                  call u_scan_c(epsc, ec_rhoa, ec_rhob, ec_sigaa, ec_sigab, ec_sigbb, ec_taua, ec_taub, &
                        rho(1, k), rho(2, k), sigma(1, k), sigma(2, k), sigma(3, k), tau(1, k), tau(2, k))
                  eps(k) = epsc
                  vrho(1, k) = ec_rhoa
                  vrho(2, k) = ec_rhob
                  vsigma(1, k) = ec_sigaa
                  vsigma(2, k) = ec_sigab
                  vsigma(3, k) = ec_sigbb
                  vlapl(1, k) = ZERO
                  vlapl(2, k) = ZERO
                  vtau(1, k) = ec_taua
                  vtau(2, k) = ec_taub

                  if (TWO*rho(1, k) > SCAN_X_RHO_THRESH) then
                        call ec_sr_scan_x(sr_eps, sr_rho, sr_sigma, sr_lapl, sr_tau, TWO*rho(1, k), FOUR*sigma(1, k), &
                              TWO*lapl(1, k), TWO*tau(1, k), omega)
                        eps(k) = eps(k) + xfrac * (rho(1, k) / (rho(1, k) + rho(2, k))) * sr_eps
                        vrho(1, k) = vrho(1, k) + xfrac * sr_rho
                        vsigma(1, k) = vsigma(1, k) + TWO * xfrac * sr_sigma
                        vlapl(1, k) = vlapl(1, k) + xfrac * sr_lapl
                        vtau(1, k) = vtau(1, k) + xfrac * sr_tau
                  end if
                  
                  if (TWO*rho(2, k) > SCAN_X_RHO_THRESH) then
                        call ec_sr_scan_x(sr_eps, sr_rho, sr_sigma, sr_lapl, sr_tau, TWO*rho(2, k), FOUR*sigma(3, k), &
                              TWO*lapl(2, k), TWO*tau(2, k), omega)
                        eps(k) = eps(k) + xfrac * (rho(2, k) / (rho(1, k) + rho(2, k))) * sr_eps
                        vrho(2, k) = vrho(2, k) + xfrac * sr_rho
                        vsigma(3, k) = vsigma(3, k) + TWO * xfrac * sr_sigma
                        vlapl(2, k) = vlapl(2, k) + xfrac * sr_lapl
                        vtau(2, k) = vtau(2, k) + xfrac * sr_tau
                  end if
            end do
      end subroutine u_ec_scan_xc


      subroutine ec_sh_scan_xc(eps, vrho, vsigma, vlapl, vtau, rho, sigma, lapl, tau, npt, srexx, omega)
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
            real(F64) :: epsx, ex_rho, ex_sigma, ex_lapl, ex_tau, srfrac
            real(F64) :: epsc, ec_rho, ec_sigma, ec_tau

            srfrac = ONE - srexx
            do k = 1, npt
                  call scan_c(epsc, ec_rho, ec_sigma, ec_tau, rho(k), sigma(k), tau(k))
                  if (rho(k) > SCAN_X_RHO_THRESH) then
                        call ec_sh_scan_x(epsx, ex_rho, ex_sigma, ex_lapl, ex_tau, rho(k), sigma(k), lapl(k), tau(k), srfrac, omega)
                  else
                        epsx = ZERO
                        ex_rho = ZERO
                        ex_sigma = ZERO
                        ex_lapl = ZERO
                        ex_tau = ZERO
                  end if
                  eps(k) = epsc + epsx
                  vrho(k) = ec_rho + ex_rho
                  vsigma(k) = ec_sigma + ex_sigma
                  vlapl(k) = ex_lapl
                  vtau(k) = ec_tau + ex_tau
            end do
      end subroutine ec_sh_scan_xc


      subroutine u_ec_sh_scan_xc(eps, vrho, vsigma, vlapl, vtau, rho, sigma, lapl, tau, npt, srexx, omega)
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
            real(F64) :: epsx, ex_rho, ex_sigma, ex_lapl, ex_tau, srfrac
            real(F64) :: epsc, ec_rhoa, ec_rhob, ec_sigaa, ec_sigab, ec_sigbb, ec_taua, ec_taub

            srfrac = ONE - srexx
            do k = 1, npt
                  call u_scan_c(epsc, ec_rhoa, ec_rhob, ec_sigaa, ec_sigab, ec_sigbb, ec_taua, ec_taub, &
                        rho(1, k), rho(2, k), sigma(1, k), sigma(2, k), sigma(3, k), tau(1, k), tau(2, k))
                  eps(k) = epsc
                  vrho(1, k) = ec_rhoa
                  vrho(2, k) = ec_rhob
                  vsigma(1, k) = ec_sigaa
                  vsigma(2, k) = ec_sigab
                  vsigma(3, k) = ec_sigbb
                  vlapl(1, k) = ZERO
                  vlapl(2, k) = ZERO
                  vtau(1, k) = ec_taua
                  vtau(2, k) = ec_taub

                  if (TWO*rho(1, k) > SCAN_X_RHO_THRESH) then
                        call ec_sh_scan_x(epsx, ex_rho, ex_sigma, ex_lapl, ex_tau, TWO*rho(1, k), FOUR*sigma(1, k), &
                              TWO*lapl(1, k), TWO*tau(1, k), srfrac, omega)
                        eps(k) = eps(k) + (rho(1, k) / (rho(1, k) + rho(2, k))) * epsx
                        vrho(1, k) = vrho(1, k) + ex_rho
                        vsigma(1, k) = vsigma(1, k) + TWO * ex_sigma
                        vlapl(1, k) = vlapl(1, k) + ex_lapl
                        vtau(1, k) = vtau(1, k) + ex_tau
                  end if
                  
                  if (TWO*rho(2, k) > SCAN_X_RHO_THRESH) then
                        call ec_sh_scan_x(epsx, ex_rho, ex_sigma, ex_lapl, ex_tau, TWO*rho(2, k), FOUR*sigma(3, k), &
                              TWO*lapl(2, k), TWO*tau(2, k), srfrac, omega)
                        eps(k) = eps(k) + (rho(2, k) / (rho(1, k) + rho(2, k))) * epsx
                        vrho(2, k) = vrho(2, k) + ex_rho
                        vsigma(3, k) = vsigma(3, k) + TWO * ex_sigma
                        vlapl(2, k) = vlapl(2, k) + ex_lapl
                        vtau(2, k) = vtau(2, k) + ex_tau
                  end if
            end do
      end subroutine u_ec_sh_scan_xc
end module scan_xc_energy
