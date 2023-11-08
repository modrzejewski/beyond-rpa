module mvs_xc_energy
      use arithmetic
      use math_constants
      use mvs_x_energy
      use pbe_c_energy

      implicit none

contains

      subroutine ec_mvs_xc(eps, vrho, vsigma, vlapl, vtau, rho, sigma, lapl, tau, npt, srexx, omega)
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
            real(F64) :: epsc, ec_rho, ec_sigma

            xfrac = ONE - srexx
            do k = 1, npt
                  call mvs_c(epsc, ec_rho, ec_sigma, rho(k), sigma(k))
                  if (rho(k) > MVS_X_RHO_THRESH) then
                        call ec_sr_mvs_x(sr_eps, sr_rho, sr_sigma, sr_lapl, sr_tau, rho(k), sigma(k), lapl(k), tau(k), omega)
                  else
                        sr_eps = ZERO
                        sr_rho = ZERO
                        sr_sigma = ZERO
                        sr_lapl = ZERO
                        sr_tau = ZERO
                  end if
                  eps = epsc + xfrac * sr_eps
                  vrho(k) = ec_rho + xfrac * sr_rho
                  vsigma(k) = ec_sigma + xfrac * sr_sigma
                  vlapl(k) = xfrac * sr_lapl
                  vtau(k) = xfrac * sr_tau
            end do
      end subroutine ec_mvs_xc


      subroutine u_ec_mvs_xc(eps, vrho, vsigma, vlapl, vtau, rho, sigma, lapl, tau, npt, srexx, omega)
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
                  call u_mvs_c(epsc, ec_rhoa, ec_rhob, ec_sigaa, ec_sigab, ec_sigbb, &
                        rho(1, k), rho(2, k), sigma(1, k), sigma(2, k), sigma(3, k))
                  eps(k) = epsc
                  vrho(1, k) = ec_rhoa
                  vrho(2, k) = ec_rhob
                  vsigma(1, k) = ec_sigaa
                  vsigma(2, k) = ec_sigab
                  vsigma(3, k) = ec_sigbb
                  vlapl(1, k) = ZERO
                  vlapl(2, k) = ZERO
                  vtau(1, k) = ZERO
                  vtau(2, k) = ZERO

                  if (TWO*rho(1, k) > MVS_X_RHO_THRESH) then
                        call ec_sr_mvs_x(sr_eps, sr_rho, sr_sigma, sr_lapl, sr_tau, TWO*rho(1, k), FOUR*sigma(1, k), &
                              TWO*lapl(1, k), TWO*tau(1, k), omega)
                        eps(k) = eps(k) + xfrac * (rho(1, k) / (rho(1, k) + rho(2, k))) * sr_eps
                        vrho(1, k) = vrho(1, k) + xfrac * sr_rho
                        vsigma(1, k) = vsigma(1, k) + TWO * xfrac * sr_sigma
                        vlapl(1, k) = vlapl(1, k) + xfrac * sr_lapl
                        vtau(1, k) = vtau(1, k) + xfrac * sr_tau
                  end if
                  
                  if (TWO*rho(2, k) > MVS_X_RHO_THRESH) then
                        call ec_sr_mvs_x(sr_eps, sr_rho, sr_sigma, sr_lapl, sr_tau, TWO*rho(2, k), FOUR*sigma(3, k), &
                              TWO*lapl(2, k), TWO*tau(2, k), omega)
                        eps(k) = eps(k) + xfrac * (rho(2, k) / (rho(1, k) + rho(2, k))) * sr_eps
                        vrho(2, k) = vrho(2, k) + xfrac * sr_rho
                        vsigma(3, k) = vsigma(3, k) + TWO * xfrac * sr_sigma
                        vlapl(2, k) = vlapl(2, k) + xfrac * sr_lapl
                        vtau(2, k) = vtau(2, k) + xfrac * sr_tau
                  end if
            end do
      end subroutine u_ec_mvs_xc

      
      subroutine mvs_xc(eps, vrho, vsigma, vtau, rho, sigma, tau, npt, exx)
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
            real(F64) :: slx, epsx, ex_rho, ex_sigma, ex_tau, epsc, ec_rho, ec_sigma

            slx = ONE - exx
            do k = 1, npt
                  if (rho(k) > TWO*MVS_X_RHO_THRESH) then
                        call mvs_x(epsx, ex_rho, ex_sigma, ex_tau, rho(k), sigma(k), tau(k))
                        call mvs_c(epsc, ec_rho, ec_sigma, rho(k), sigma(k))
                        eps(k) = slx * epsx + epsc
                        vrho(k) = slx * ex_rho + ec_rho
                        vsigma(k) = slx * ex_sigma + ec_sigma
                        vtau(k) = slx * ex_tau
                  end if
            end do
      end subroutine mvs_xc


      subroutine u_mvs_xc(eps, vrho, vsigma, vtau, rho, sigma, tau, npt, exx)
            real(F64), dimension(:), intent(out)    :: eps
            real(F64), dimension(:, :), intent(out) :: vrho
            real(F64), dimension(:, :), intent(out) :: vsigma
            real(F64), dimension(:, :), intent(out) :: vtau
            real(F64), dimension(:, :), intent(in)  :: rho
            real(F64), dimension(:, :), intent(in)  :: sigma
            real(F64), dimension(:, :), intent(in)  :: tau
            integer, intent(in)                     :: npt
            real(F64), intent(in)                   :: exx

            real(F64) :: rhoa2, sigma_aa4, taua2, rhob2, sigma_bb4, taub2
            real(F64) :: epsa, ex_rhoa, ex_sigaa, ex_taua
            real(F64) :: epsb, ex_rhob, ex_sigbb, ex_taub
            real(F64) :: epsc, ec_rhoa, ec_rhob, ec_sigaa, ec_sigab, ec_sigbb
            real(F64) :: slx, rhotot
            integer :: k

            slx = ONE - exx
            do k = 1, npt
                  vsigma(2, k) = ZERO
                  rhotot = rho(1, k) + rho(2, k)
                  !
                  ! Alpha contribution
                  !
                  if (rho(1, k) > MVS_X_RHO_THRESH) then
                        rhoa2 = rho(1, k) * TWO
                        sigma_aa4 = sigma(1, k) * FOUR
                        taua2 = tau(1, k) * TWO
                        call mvs_x(epsa, ex_rhoa, ex_sigaa, ex_taua, rhoa2, sigma_aa4, taua2)
                        ex_sigaa = ex_sigaa * TWO
                        vrho(1, k) = slx * ex_rhoa
                        vsigma(1, k) = slx * ex_sigaa
                        vtau(1, k) = slx * ex_taua
                        eps(k) = (rho(1, k)/rhotot) * slx * epsa
                  else
                        eps(k) = ZERO
                        vrho(1, k) = ZERO
                        vsigma(1, k) = ZERO
                        vtau(1, k) = ZERO
                  end if
                  !
                  ! Beta contribution
                  !
                  if (rho(2, k) > MVS_X_RHO_THRESH) then
                        rhob2 = rho(2, k) * TWO
                        sigma_bb4 = sigma(3, k) * FOUR
                        taub2 = tau(2, k) * TWO                        
                        call mvs_x(epsb, ex_rhob, ex_sigbb, ex_taub, rhob2, sigma_bb4, taub2)
                        ex_sigbb = ex_sigbb * TWO
                        vrho(2, k) = slx * ex_rhob
                        vsigma(3, k) = slx * ex_sigbb
                        vtau(2, k) = slx * ex_taub
                        eps(k) = eps(k) + (rho(2, k)/rhotot) * slx * epsb
                  else
                        vrho(2, k) = ZERO
                        vsigma(3, k) = ZERO
                        vtau(2, k) = ZERO
                  end if

                  call u_mvs_c(epsc, ec_rhoa, ec_rhob, ec_sigaa, ec_sigab, ec_sigbb, &
                        rho(1, k), rho(2, k), sigma(1, k), sigma(2, k), sigma(3, k))
                  eps(k) = eps(k) + epsc
                  vrho(1, k) = vrho(1, k) + ec_rhoa
                  vrho(2, k) = vrho(2, k) + ec_rhob
                  vsigma(1, k) = vsigma(1, k) + ec_sigaa
                  vsigma(2, k) = vsigma(2, k) + ec_sigab
                  vsigma(3, k) = vsigma(3, k) + ec_sigbb
            end do
      end subroutine u_mvs_xc
end module mvs_xc_energy
