module modrzej2016_xc_energy
      use math_constants
      use arithmetic
      use modrzej2016_x_energy
      use modrzej2016_c_energy
      use modrzej2012_c_energy
      use modrzej2014_xc_energy
      use lda_x_energy
      use lda_c_energy
      use pbe_x_energy
      use tpss_c_energy
      use b88_x_energy
      use hjs_x_energy

      implicit none

contains

      subroutine u_mcsv2_xc(eps, vrho, vsigma, vlapl, vtau, rho, sigma, lapl, tau, npt, srexx, omega)
            !
            ! Compute the MCSv2 exchange-correlation energy for spin-uncompensated densities
            !
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
                  call u_modrzej2016_acd_c_tpss(epsc, ec_rhoa, ec_rhob, ec_sigaa, ec_sigab, ec_sigbb, ec_taua, ec_taub, &
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
                  
                  if (TWO*rho(1, k) > B88_X_RHO_THRESH) then
                        call ec_sr_b88_x(sr_eps, sr_rho, sr_sigma, sr_lapl, sr_tau, &
                              TWO*rho(1, k), FOUR*sigma(1, k), TWO*lapl(1, k), TWO*tau(1, k), omega)
                        eps(k) = eps(k) + xfrac * (rho(1, k) / (rho(1, k) + rho(2, k))) * sr_eps
                        vrho(1, k) = vrho(1, k) + xfrac * sr_rho
                        vsigma(1, k) = vsigma(1, k) + TWO * xfrac * sr_sigma
                        vlapl(1, k) = vlapl(1, k) + xfrac * sr_lapl
                        vtau(1, k) = vtau(1, k) + xfrac * sr_tau
                  end if

                  if (TWO*rho(2, k) > B88_X_RHO_THRESH) then
                        call ec_sr_b88_x(sr_eps, sr_rho, sr_sigma, sr_lapl, sr_tau, &
                              TWO*rho(2, k), FOUR*sigma(3, k), TWO*lapl(2, k), TWO*tau(2, k), omega)
                        eps(k) = eps(k) + xfrac * (rho(2, k) / (rho(1, k) + rho(2, k))) * sr_eps
                        vrho(2, k) = vrho(2, k) + xfrac * sr_rho
                        vsigma(3, k) = vsigma(3, k) + TWO * xfrac * sr_sigma
                        vlapl(2, k) = vlapl(2, k) + xfrac * sr_lapl
                        vtau(2, k) = vtau(2, k) + xfrac * sr_tau
                  end if                  
            end do
      end subroutine u_mcsv2_xc


      subroutine u_mcsv3_xc(eps, vrho, vsigma, vlapl, vtau, rho, sigma, lapl, tau, npt, srexx, omega)
            !
            ! Compute the MCSv2 exchange-correlation energy for spin-uncompensated densities
            !
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
                  ! call u_mlrcs12_c(epsc, ec_rhoa, ec_rhob, ec_sigaa, ec_sigab, ec_sigbb, ec_taua, ec_taub, &
                  !       rho(1, k), rho(2, k), sigma(1, k), sigma(2, k), sigma(3, k), tau(1, k), tau(2, k), MCSv4_GPARAM)
                  call u_modrzej2016_acd_c_tpss(epsc, ec_rhoa, ec_rhob, ec_sigaa, ec_sigab, ec_sigbb, ec_taua, ec_taub, &
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
                  
                  if (TWO*rho(1, k) > PBE_X_RHO_THRESH) then
                        call ec_sr_pbe_x(sr_eps, sr_rho, sr_sigma, sr_lapl, sr_tau, &
                              TWO*rho(1, k), FOUR*sigma(1, k), TWO*lapl(1, k), TWO*tau(1, k), omega)
                        eps(k) = eps(k) + xfrac * (rho(1, k) / (rho(1, k) + rho(2, k))) * sr_eps
                        vrho(1, k) = vrho(1, k) + xfrac * sr_rho
                        vsigma(1, k) = vsigma(1, k) + TWO * xfrac * sr_sigma
                        vlapl(1, k) = vlapl(1, k) + xfrac * sr_lapl
                        vtau(1, k) = vtau(1, k) + xfrac * sr_tau
                  end if

                  if (TWO*rho(2, k) > PBE_X_RHO_THRESH) then
                        call ec_sr_pbe_x(sr_eps, sr_rho, sr_sigma, sr_lapl, sr_tau, &
                              TWO*rho(2, k), FOUR*sigma(3, k), TWO*lapl(2, k), TWO*tau(2, k), omega)
                        eps(k) = eps(k) + xfrac * (rho(2, k) / (rho(1, k) + rho(2, k))) * sr_eps
                        vrho(2, k) = vrho(2, k) + xfrac * sr_rho
                        vsigma(3, k) = vsigma(3, k) + TWO * xfrac * sr_sigma
                        vlapl(2, k) = vlapl(2, k) + xfrac * sr_lapl
                        vtau(2, k) = vtau(2, k) + xfrac * sr_tau
                  end if                  
            end do
      end subroutine u_mcsv3_xc


      subroutine mcsv2_xc(eps, vrho, vsigma, vlapl, vtau, rho, sigma, lapl, tau, npt, srexx, omega)
            !
            ! Compute the MCSv2 exchange-correlation energy for spin-compensated densities
            !
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
                  call modrzej2016_acd_c_tpss(eps(k), vrho(k), vsigma(k), vtau(k), rho(k), sigma(k), tau(k))
                  vlapl(k) = ZERO
                  if (rho(k) > B88_X_RHO_THRESH) then
                        call ec_sr_b88_x(sr_eps, sr_rho, sr_sigma, sr_lapl, sr_tau, &
                              rho(k), sigma(k), lapl(k), tau(k), omega)
                        eps(k) = eps(k) + xfrac * sr_eps
                        vrho(k) = vrho(k) + xfrac * sr_rho
                        vsigma(k) = vsigma(k) + xfrac * sr_sigma
                        vlapl(k) = vlapl(k) + xfrac * sr_lapl
                        vtau(k) = vtau(k) + xfrac * sr_tau
                  end if
            end do
      end subroutine mcsv2_xc


      subroutine mcsv3_xc(eps, vrho, vsigma, vlapl, vtau, rho, sigma, lapl, tau, npt, srexx, omega)
            !
            ! Compute the MCSv3 exchange-correlation energy for spin-compensated densities
            !
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
!                  call mlrcs12_c(eps(k), vrho(k), vsigma(k), vtau(k), rho(k), sigma(k), tau(k), MCSv4_GPARAM)
                  call modrzej2016_acd_c_tpss(eps(k), vrho(k), vsigma(k), vtau(k), rho(k), sigma(k), tau(k))
                  vlapl(k) = ZERO
                  if (rho(k) > PBE_X_RHO_THRESH) then
                        call ec_sr_pbe_x(sr_eps, sr_rho, sr_sigma, sr_lapl, sr_tau, &
                              rho(k), sigma(k), lapl(k), tau(k), omega)
                        eps(k) = eps(k) + xfrac * sr_eps
                        vrho(k) = vrho(k) + xfrac * sr_rho
                        vsigma(k) = vsigma(k) + xfrac * sr_sigma
                        vlapl(k) = vlapl(k) + xfrac * sr_lapl
                        vtau(k) = vtau(k) + xfrac * sr_tau
                  end if
            end do
      end subroutine mcsv3_xc


      subroutine ec_pbetpss_xc(eps, vrho, vsigma, vlapl, vtau, rho, sigma, lapl, tau, npt, srexx, omega)
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
                  call tpss_c(epsc, ec_rho, ec_sigma, ec_tau, rho(k), sigma(k), tau(k))
                  if (rho(k) > PBE_X_RHO_THRESH) then
                        call ec_sr_pbe_x(sr_eps, sr_rho, sr_sigma, sr_lapl, sr_tau, rho(k), sigma(k), lapl(k), tau(k), omega)
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
      end subroutine ec_pbetpss_xc


      subroutine u_ec_pbetpss_xc(eps, vrho, vsigma, vlapl, vtau, rho, sigma, lapl, tau, npt, srexx, omega)
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
                  call u_tpss_c(epsc, ec_rhoa, ec_rhob, ec_sigaa, ec_sigab, ec_sigbb, ec_taua, ec_taub, &
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

                  if (TWO*rho(1, k) > PBE_X_RHO_THRESH) then
                        call ec_sr_pbe_x(sr_eps, sr_rho, sr_sigma, sr_lapl, sr_tau, TWO*rho(1, k), FOUR*sigma(1, k), &
                              TWO*lapl(1, k), TWO*tau(1, k), omega)
                        eps(k) = eps(k) + xfrac * (rho(1, k) / (rho(1, k) + rho(2, k))) * sr_eps
                        vrho(1, k) = vrho(1, k) + xfrac * sr_rho
                        vsigma(1, k) = vsigma(1, k) + TWO * xfrac * sr_sigma
                        vlapl(1, k) = vlapl(1, k) + xfrac * sr_lapl
                        vtau(1, k) = vtau(1, k) + xfrac * sr_tau
                  end if
                  
                  if (TWO*rho(2, k) > PBE_X_RHO_THRESH) then
                        call ec_sr_pbe_x(sr_eps, sr_rho, sr_sigma, sr_lapl, sr_tau, TWO*rho(2, k), FOUR*sigma(3, k), &
                              TWO*lapl(2, k), TWO*tau(2, k), omega)
                        eps(k) = eps(k) + xfrac * (rho(2, k) / (rho(1, k) + rho(2, k))) * sr_eps
                        vrho(2, k) = vrho(2, k) + xfrac * sr_rho
                        vsigma(3, k) = vsigma(3, k) + TWO * xfrac * sr_sigma
                        vlapl(2, k) = vlapl(2, k) + xfrac * sr_lapl
                        vtau(2, k) = vtau(2, k) + xfrac * sr_tau
                  end if
            end do
      end subroutine u_ec_pbetpss_xc


      subroutine ec_ldatpss_xc(eps, vrho, vsigma, vlapl, vtau, rho, sigma, lapl, tau, npt, srexx, omega)
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
                  call tpss_c(epsc, ec_rho, ec_sigma, ec_tau, rho(k), sigma(k), tau(k))
                  if (rho(k) > LDA_X_RHO_THRESH) then
                        call ec_sr_lda_x(sr_eps, sr_rho, sr_sigma, sr_lapl, sr_tau, rho(k), sigma(k), lapl(k), tau(k), omega)
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
      end subroutine ec_ldatpss_xc


      subroutine u_ec_ldatpss_xc(eps, vrho, vsigma, vlapl, vtau, rho, sigma, lapl, tau, npt, srexx, omega)
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
                  call u_tpss_c(epsc, ec_rhoa, ec_rhob, ec_sigaa, ec_sigab, ec_sigbb, ec_taua, ec_taub, &
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

                  if (TWO*rho(1, k) > LDA_X_RHO_THRESH) then
                        call ec_sr_lda_x(sr_eps, sr_rho, sr_sigma, sr_lapl, sr_tau, TWO*rho(1, k), FOUR*sigma(1, k), &
                              TWO*lapl(1, k), TWO*tau(1, k), omega)
                        eps(k) = eps(k) + xfrac * (rho(1, k) / (rho(1, k) + rho(2, k))) * sr_eps
                        vrho(1, k) = vrho(1, k) + xfrac * sr_rho
                        vsigma(1, k) = vsigma(1, k) + TWO * xfrac * sr_sigma
                        vlapl(1, k) = vlapl(1, k) + xfrac * sr_lapl
                        vtau(1, k) = vtau(1, k) + xfrac * sr_tau
                  end if
                  
                  if (TWO*rho(2, k) > LDA_X_RHO_THRESH) then
                        call ec_sr_lda_x(sr_eps, sr_rho, sr_sigma, sr_lapl, sr_tau, TWO*rho(2, k), FOUR*sigma(3, k), &
                              TWO*lapl(2, k), TWO*tau(2, k), omega)
                        eps(k) = eps(k) + xfrac * (rho(2, k) / (rho(1, k) + rho(2, k))) * sr_eps
                        vrho(2, k) = vrho(2, k) + xfrac * sr_rho
                        vsigma(3, k) = vsigma(3, k) + TWO * xfrac * sr_sigma
                        vlapl(2, k) = vlapl(2, k) + xfrac * sr_lapl
                        vtau(2, k) = vtau(2, k) + xfrac * sr_tau
                  end if
            end do
      end subroutine u_ec_ldatpss_xc


      subroutine ec_lda_xc(eps, vrho, vsigma, vlapl, vtau, rho, sigma, lapl, tau, npt, srexx, omega)
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
            real(F64) :: epsc, ec_rho

            xfrac = ONE - srexx
            do k = 1, npt
                  call lda_c(epsc, ec_rho, rho(k))
                  if (rho(k) > LDA_X_RHO_THRESH) then
                        call ec_sr_lda_x(sr_eps, sr_rho, sr_sigma, sr_lapl, sr_tau, rho(k), sigma(k), lapl(k), tau(k), omega)
                  else
                        sr_eps = ZERO
                        sr_rho = ZERO
                        sr_sigma = ZERO
                        sr_lapl = ZERO
                        sr_tau = ZERO
                  end if
                  eps(k) = epsc + xfrac * sr_eps
                  vrho(k) = ec_rho + xfrac * sr_rho
                  vsigma(k) = xfrac * sr_sigma
                  vlapl(k) = xfrac * sr_lapl
                  vtau(k) = xfrac * sr_tau
            end do
      end subroutine ec_lda_xc


      subroutine u_ec_lda_xc(eps, vrho, vsigma, vlapl, vtau, rho, sigma, lapl, tau, npt, srexx, omega)
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
            real(F64) :: epsc, ec_rhoa, ec_rhob

            xfrac = ONE - srexx
            do k = 1, npt
                  call u_lda_c(epsc, ec_rhoa, ec_rhob, rho(1, k), rho(2, k))
                  eps(k) = epsc
                  vrho(1, k) = ec_rhoa
                  vrho(2, k) = ec_rhob
                  vsigma(1, k) = ZERO
                  vsigma(2, k) = ZERO
                  vsigma(3, k) = ZERO
                  vlapl(1, k) = ZERO
                  vlapl(2, k) = ZERO
                  vtau(1, k) = ZERO
                  vtau(2, k) = ZERO

                  if (TWO*rho(1, k) > LDA_X_RHO_THRESH) then
                        call ec_sr_lda_x(sr_eps, sr_rho, sr_sigma, sr_lapl, sr_tau, TWO*rho(1, k), FOUR*sigma(1, k), &
                              TWO*lapl(1, k), TWO*tau(1, k), omega)
                        eps(k) = eps(k) + xfrac * (rho(1, k) / (rho(1, k) + rho(2, k))) * sr_eps
                        vrho(1, k) = vrho(1, k) + xfrac * sr_rho
                        vsigma(1, k) = vsigma(1, k) + TWO * xfrac * sr_sigma
                        vlapl(1, k) = vlapl(1, k) + xfrac * sr_lapl
                        vtau(1, k) = vtau(1, k) + xfrac * sr_tau
                  end if
                  
                  if (TWO*rho(2, k) > LDA_X_RHO_THRESH) then
                        call ec_sr_lda_x(sr_eps, sr_rho, sr_sigma, sr_lapl, sr_tau, TWO*rho(2, k), FOUR*sigma(3, k), &
                              TWO*lapl(2, k), TWO*tau(2, k), omega)
                        eps(k) = eps(k) + xfrac * (rho(2, k) / (rho(1, k) + rho(2, k))) * sr_eps
                        vrho(2, k) = vrho(2, k) + xfrac * sr_rho
                        vsigma(3, k) = vsigma(3, k) + TWO * xfrac * sr_sigma
                        vlapl(2, k) = vlapl(2, k) + xfrac * sr_lapl
                        vtau(2, k) = vtau(2, k) + xfrac * sr_tau
                  end if
            end do
      end subroutine u_ec_lda_xc
end module modrzej2016_xc_energy
