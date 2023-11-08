module hjs_xc_energy
      use arithmetic
      use math_constants
      use hjs_x_energy
      use pbe_c_energy
      use pbe_x_energy
      use tpss_c_energy
      use lyp_c_energy

      implicit none

contains

      subroutine hjs_pbe_xc(eps, e_rho, e_sigma, rho, sigma, npt, srexx, omega)
            !
            ! Compute the LC-wPBE exchange-correlation energy with the short-range
            ! exchange of Henderson, Janesko, and Scuseria.
            !
            real(F64), dimension(:), intent(out) :: eps
            real(F64), dimension(:), intent(out) :: e_rho
            real(F64), dimension(:), intent(out) :: e_sigma
            real(F64), dimension(:), intent(in)  :: rho
            real(F64), dimension(:), intent(in)  :: sigma
            integer, intent(in)                  :: npt
            real(F64), intent(in)                :: srexx
            real(F64), intent(in)                :: omega

            integer :: k
            real(F64) :: epsx, ex_rho, ex_sigma, xfrac
            real(F64) :: epsc, ec_rho, ec_sigma
            
            xfrac = ONE - srexx
            do k = 1, npt
                  if (rho(k) > HJS_X_RHO_THRESH) then
                        call hjs_sr_pbe_x(epsx, ex_rho, ex_sigma, rho(k), sigma(k), omega)
                        call pbe_c(epsc, ec_rho, ec_sigma, rho(k), sigma(k))
                        eps(k) = xfrac * epsx + epsc
                        e_rho(k) = xfrac * ex_rho + ec_rho
                        e_sigma(k) = xfrac * ex_sigma + ec_sigma
                  else
                        eps(k) = ZERO
                        e_rho(k) = ZERO
                        e_sigma(k) = ZERO
                  end if
            end do
      end subroutine hjs_pbe_xc


      subroutine hjs_sh_pbe_xc(eps, e_rho, e_sigma, rho, sigma, npt, srexx, omega)
            !
            ! The screened hybrid version of the LC-wPBE exchange-correlation
            ! energy based on the exchange hole of Henderson, Janesko, and Scuseria.
            !
            real(F64), dimension(:), intent(out) :: eps
            real(F64), dimension(:), intent(out) :: e_rho
            real(F64), dimension(:), intent(out) :: e_sigma
            real(F64), dimension(:), intent(in)  :: rho
            real(F64), dimension(:), intent(in)  :: sigma
            integer, intent(in)                  :: npt
            real(F64), intent(in)                :: srexx
            real(F64), intent(in)                :: omega

            integer :: k
            real(F64) :: epsx, ex_rho, ex_sigma, srfrac
            real(F64) :: epsc, ec_rho, ec_sigma
            
            srfrac = ONE - srexx
            do k = 1, npt
                  if (rho(k) > HJS_X_RHO_THRESH) then
                        call hjs_sh_pbe_x(epsx, ex_rho, ex_sigma, rho(k), sigma(k), srfrac, omega)
                        call pbe_c(epsc, ec_rho, ec_sigma, rho(k), sigma(k))
                        eps(k) = epsx + epsc
                        e_rho(k) = ex_rho + ec_rho
                        e_sigma(k) = ex_sigma + ec_sigma
                  else
                        eps(k) = ZERO
                        e_rho(k) = ZERO
                        e_sigma(k) = ZERO
                  end if
            end do
      end subroutine hjs_sh_pbe_xc
      

      subroutine hjs_pbetpss_xc(eps, e_rho, e_sigma, e_tau, rho, sigma, tau, npt, srexx, omega)
            !
            ! Compute the LC-PBETPSS exchange-correlation energy with the short-range
            ! exchange of Henderson, Janesko, and Scuseria.
            !
            real(F64), dimension(:), intent(out) :: eps
            real(F64), dimension(:), intent(out) :: e_rho
            real(F64), dimension(:), intent(out) :: e_sigma
            real(F64), dimension(:), intent(out) :: e_tau
            real(F64), dimension(:), intent(in)  :: rho
            real(F64), dimension(:), intent(in)  :: sigma
            real(F64), dimension(:), intent(in)  :: tau
            integer, intent(in)                  :: npt
            real(F64), intent(in)                :: srexx
            real(F64), intent(in)                :: omega

            integer :: k
            real(F64) :: epsx, ex_rho, ex_sigma, xfrac
            real(F64) :: epsc, ec_rho, ec_sigma, ec_tau
            
            xfrac = ONE - srexx
            do k = 1, npt
                  if (rho(k) > HJS_X_RHO_THRESH) then
                        call hjs_sr_pbe_x(epsx, ex_rho, ex_sigma, rho(k), sigma(k), omega)
                        call tpss_c(epsc, ec_rho, ec_sigma, ec_tau, rho(k), sigma(k), tau(k))
                        eps(k) = xfrac * epsx + epsc
                        e_rho(k) = xfrac * ex_rho + ec_rho
                        e_sigma(k) = xfrac * ex_sigma + ec_sigma
                        e_tau(k) = ec_tau
                  else
                        eps(k) = ZERO
                        e_rho(k) = ZERO
                        e_sigma(k) = ZERO
                        e_tau(k) = ZERO
                  end if
            end do
      end subroutine hjs_pbetpss_xc


      subroutine u_hjs_pbetpss_xc(eps, e_rho, e_sigma, e_tau, rho, sigma, tau, npt, srexx, omega)
            !
            ! Compute the LC-PBETPSS exchange-correlation energy with the short-range
            ! exchange of Henderson, Janesko, and Scuseria.
            !
            real(F64), dimension(:), intent(out)    :: eps
            real(F64), dimension(:, :), intent(out) :: e_rho
            real(F64), dimension(:, :), intent(out) :: e_sigma
            real(F64), dimension(:, :), intent(out) :: e_tau
            real(F64), dimension(:, :), intent(in)  :: rho
            real(F64), dimension(:, :), intent(in)  :: sigma
            real(F64), dimension(:, :), intent(in)  :: tau
            integer, intent(in)                     :: npt
            real(F64), intent(in)                   :: srexx
            real(F64), intent(in)                   :: omega

            integer :: k
            real(F64) :: epsx, ex_rho, ex_sigma, xfrac
            real(F64) :: epsc, ec_rhoa, ec_rhob, ec_sigaa, ec_sigab, ec_sigbb, ec_taua, ec_taub

            xfrac = ONE - srexx
            do k = 1, npt
                  call u_tpss_c(epsc, ec_rhoa, ec_rhob, ec_sigaa, ec_sigab, ec_sigbb, ec_taua, ec_taub, &
                        rho(1, k), rho(2, k), sigma(1, k), sigma(2, k), sigma(3, k), tau(1, k), tau(2, k))
                  eps(k) = epsc
                  e_rho(1, k) = ec_rhoa
                  e_rho(2, k) = ec_rhob
                  e_sigma(1, k) = ec_sigaa
                  e_sigma(2, k) = ec_sigab
                  e_sigma(3, k) = ec_sigbb
                  e_tau(1, k) = ec_taua
                  e_tau(2, k) = ec_taub

                  if (TWO*rho(1, k) > HJS_X_RHO_THRESH) then
                        call hjs_sr_pbe_x(epsx, ex_rho, ex_sigma, TWO*rho(1, k), FOUR*sigma(1, k), omega)
                        eps(k) = eps(k) + xfrac * (rho(1, k) / (rho(1, k) + rho(2, k))) * epsx
                        e_rho(1, k) = e_rho(1, k) + xfrac * ex_rho
                        e_sigma(1, k) = e_sigma(1, k) + TWO * xfrac * ex_sigma
                  end if

                  if (TWO*rho(2, k) > HJS_X_RHO_THRESH) then
                        call hjs_sr_pbe_x(epsx, ex_rho, ex_sigma, TWO*rho(2, k), FOUR*sigma(3, k), omega)
                        eps(k) = eps(k) + xfrac * (rho(2, k) / (rho(1, k) + rho(2, k))) * epsx
                        e_rho(2, k) = e_rho(2, k) + xfrac * ex_rho
                        e_sigma(3, k) = e_sigma(3, k) + TWO * xfrac * ex_sigma
                  end if                  
            end do
      end subroutine u_hjs_pbetpss_xc


      subroutine u_hjs_pbe_xc(eps, e_rho, e_sigma, rho, sigma, npt, srexx, omega)
            !
            ! Compute the LC-wPBE exchange-correlation energy with the short-range
            ! exchange of Henderson, Janesko, and Scuseria.
            !
            real(F64), dimension(:), intent(out)    :: eps
            real(F64), dimension(:, :), intent(out) :: e_rho
            real(F64), dimension(:, :), intent(out) :: e_sigma
            real(F64), dimension(:, :), intent(in)  :: rho
            real(F64), dimension(:, :), intent(in)  :: sigma
            integer, intent(in)                     :: npt
            real(F64), intent(in)                   :: srexx
            real(F64), intent(in)                   :: omega

            integer :: k
            real(F64) :: epsx, ex_rho, ex_sigma, xfrac
            real(F64) :: epsc, ec_rhoa, ec_rhob, ec_sigaa, ec_sigab, ec_sigbb

            xfrac = ONE - srexx
            do k = 1, npt
                  call u_pbe_c(epsc, ec_rhoa, ec_rhob, ec_sigaa, ec_sigab, ec_sigbb, rho(1, k), rho(2, k), &
                        sigma(1, k), sigma(2, k), sigma(3, k))
                  eps(k) = epsc
                  e_rho(1, k) = ec_rhoa
                  e_rho(2, k) = ec_rhob
                  e_sigma(1, k) = ec_sigaa
                  e_sigma(2, k) = ec_sigab
                  e_sigma(3, k) = ec_sigbb

                  if (TWO*rho(1, k) > HJS_X_RHO_THRESH) then
                        call hjs_sr_pbe_x(epsx, ex_rho, ex_sigma, TWO*rho(1, k), FOUR*sigma(1, k), omega)
                        eps(k) = eps(k) + xfrac * (rho(1, k) / (rho(1, k) + rho(2, k))) * epsx
                        e_rho(1, k) = e_rho(1, k) + xfrac * ex_rho
                        e_sigma(1, k) = e_sigma(1, k) + TWO * xfrac * ex_sigma
                  end if

                  if (TWO*rho(2, k) > HJS_X_RHO_THRESH) then
                        call hjs_sr_pbe_x(epsx, ex_rho, ex_sigma, TWO*rho(2, k), FOUR*sigma(3, k), omega)
                        eps(k) = eps(k) + xfrac * (rho(2, k) / (rho(1, k) + rho(2, k))) * epsx
                        e_rho(2, k) = e_rho(2, k) + xfrac * ex_rho
                        e_sigma(3, k) = e_sigma(3, k) + TWO * xfrac * ex_sigma
                  end if                  
            end do
      end subroutine u_hjs_pbe_xc


      subroutine u_hjs_sh_pbe_xc(eps, e_rho, e_sigma, rho, sigma, npt, srexx, omega)
            !
            ! Compute the LC-wPBE exchange-correlation energy with the short-range
            ! exchange of Henderson, Janesko, and Scuseria.
            !
            real(F64), dimension(:), intent(out)    :: eps
            real(F64), dimension(:, :), intent(out) :: e_rho
            real(F64), dimension(:, :), intent(out) :: e_sigma
            real(F64), dimension(:, :), intent(in)  :: rho
            real(F64), dimension(:, :), intent(in)  :: sigma
            integer, intent(in)                     :: npt
            real(F64), intent(in)                   :: srexx
            real(F64), intent(in)                   :: omega

            integer :: k
            real(F64) :: epsx, ex_rho, ex_sigma, srfrac
            real(F64) :: epsc, ec_rhoa, ec_rhob, ec_sigaa, ec_sigab, ec_sigbb

            srfrac = ONE - srexx
            do k = 1, npt
                  call u_pbe_c(epsc, ec_rhoa, ec_rhob, ec_sigaa, ec_sigab, ec_sigbb, rho(1, k), rho(2, k), &
                        sigma(1, k), sigma(2, k), sigma(3, k))
                  eps(k) = epsc
                  e_rho(1, k) = ec_rhoa
                  e_rho(2, k) = ec_rhob
                  e_sigma(1, k) = ec_sigaa
                  e_sigma(2, k) = ec_sigab
                  e_sigma(3, k) = ec_sigbb

                  if (TWO*rho(1, k) > HJS_X_RHO_THRESH) then
                        call hjs_sh_pbe_x(epsx, ex_rho, ex_sigma, TWO*rho(1, k), FOUR*sigma(1, k), srfrac, omega)
                        eps(k) = eps(k) + (rho(1, k) / (rho(1, k) + rho(2, k))) * epsx
                        e_rho(1, k) = e_rho(1, k) + ex_rho
                        e_sigma(1, k) = e_sigma(1, k) + TWO * ex_sigma
                  end if

                  if (TWO*rho(2, k) > HJS_X_RHO_THRESH) then
                        call hjs_sh_pbe_x(epsx, ex_rho, ex_sigma, TWO*rho(2, k), FOUR*sigma(3, k), srfrac, omega)
                        eps(k) = eps(k) + (rho(2, k) / (rho(1, k) + rho(2, k))) * epsx
                        e_rho(2, k) = e_rho(2, k) + ex_rho
                        e_sigma(3, k) = e_sigma(3, k) + TWO * ex_sigma
                  end if                  
            end do
      end subroutine u_hjs_sh_pbe_xc

      
      subroutine hjs_pbesol_xc(eps, e_rho, e_sigma, rho, sigma, npt, srexx, omega)
            !
            ! Compute the LC-wPBEsol exchange-correlation energy with the short-range
            ! exchange of Henderson, Janesko, and Scuseria.
            !
            real(F64), dimension(:), intent(out) :: eps
            real(F64), dimension(:), intent(out) :: e_rho
            real(F64), dimension(:), intent(out) :: e_sigma
            real(F64), dimension(:), intent(in)  :: rho
            real(F64), dimension(:), intent(in)  :: sigma
            integer, intent(in)                  :: npt
            real(F64), intent(in)                :: srexx
            real(F64), intent(in)                :: omega

            integer :: k
            real(F64) :: epsx, ex_rho, ex_sigma, xfrac
            real(F64) :: epsc, ec_rho, ec_sigma
            
            xfrac = ONE - srexx
            do k = 1, npt
                  if (rho(k) > HJS_X_RHO_THRESH) then
                        call hjs_sr_pbesol_x(epsx, ex_rho, ex_sigma, rho(k), sigma(k), omega)
                        call pbesol_c(epsc, ec_rho, ec_sigma, rho(k), sigma(k))
                        eps(k) = xfrac * epsx + epsc
                        e_rho(k) = xfrac * ex_rho + ec_rho
                        e_sigma(k) = xfrac * ex_sigma + ec_sigma
                  else
                        eps(k) = ZERO
                        e_rho(k) = ZERO
                        e_sigma(k) = ZERO
                  end if
            end do
      end subroutine hjs_pbesol_xc


      subroutine u_hjs_pbesol_xc(eps, e_rho, e_sigma, rho, sigma, npt, srexx, omega)
            !
            ! Compute the LC-wPBEsol exchange-correlation energy with the short-range
            ! exchange of Henderson, Janesko, and Scuseria.
            !
            real(F64), dimension(:), intent(out)    :: eps
            real(F64), dimension(:, :), intent(out) :: e_rho
            real(F64), dimension(:, :), intent(out) :: e_sigma
            real(F64), dimension(:, :), intent(in)  :: rho
            real(F64), dimension(:, :), intent(in)  :: sigma
            integer, intent(in)                     :: npt
            real(F64), intent(in)                   :: srexx
            real(F64), intent(in)                   :: omega

            integer :: k
            real(F64) :: epsx, ex_rho, ex_sigma, xfrac
            real(F64) :: epsc, ec_rhoa, ec_rhob, ec_sigaa, ec_sigab, ec_sigbb

            xfrac = ONE - srexx
            do k = 1, npt
                  call u_pbesol_c(epsc, ec_rhoa, ec_rhob, ec_sigaa, ec_sigab, ec_sigbb, rho(1, k), rho(2, k), &
                        sigma(1, k), sigma(2, k), sigma(3, k))
                  eps(k) = epsc
                  e_rho(1, k) = ec_rhoa
                  e_rho(2, k) = ec_rhob
                  e_sigma(1, k) = ec_sigaa
                  e_sigma(2, k) = ec_sigab
                  e_sigma(3, k) = ec_sigbb

                  if (TWO*rho(1, k) > HJS_X_RHO_THRESH) then
                        call hjs_sr_pbesol_x(epsx, ex_rho, ex_sigma, TWO*rho(1, k), FOUR*sigma(1, k), omega)
                        eps(k) = eps(k) + xfrac * (rho(1, k) / (rho(1, k) + rho(2, k))) * epsx
                        e_rho(1, k) = e_rho(1, k) + xfrac * ex_rho
                        e_sigma(1, k) = e_sigma(1, k) + TWO * xfrac * ex_sigma
                  end if

                  if (TWO*rho(2, k) > HJS_X_RHO_THRESH) then
                        call hjs_sr_pbesol_x(epsx, ex_rho, ex_sigma, TWO*rho(2, k), FOUR*sigma(3, k), omega)
                        eps(k) = eps(k) + xfrac * (rho(2, k) / (rho(1, k) + rho(2, k))) * epsx
                        e_rho(2, k) = e_rho(2, k) + xfrac * ex_rho
                        e_sigma(3, k) = e_sigma(3, k) + TWO * xfrac * ex_sigma
                  end if           
            end do
      end subroutine u_hjs_pbesol_xc


      subroutine hjs_blyp_xc(eps, vrho, vsigma, rho, sigma, npt, srexx, omega)
            real(F64), dimension(:), intent(out) :: eps
            real(F64), dimension(:), intent(out) :: vrho
            real(F64), dimension(:), intent(out) :: vsigma
            real(F64), dimension(:), intent(in)  :: rho
            real(F64), dimension(:), intent(in)  :: sigma
            integer, intent(in)                  :: npt
            real(F64), intent(in)                :: srexx
            real(F64), intent(in)                :: omega

            integer :: k
            real(F64) :: sr_eps, sr_rho, sr_sigma, xfrac
            real(F64) :: epsc, ec_rhoa, ec_rhob, ec_sigaa, ec_sigab, ec_sigbb
            real(F64) :: rho_a, rho_b, sigma_aa, sigma_ab, sigma_bb

            xfrac = ONE - srexx
            do k = 1, npt
                  if (rho(k) > HJS_X_RHO_THRESH) then
                        call hjs_sr_b88_x(sr_eps, sr_rho, sr_sigma, rho(k), sigma(k), omega)
                        rho_a = rho(k) / TWO
                        rho_b = rho(k) / TWO
                        sigma_aa = sigma(k) / FOUR
                        sigma_ab = sigma(k) / FOUR
                        sigma_bb = sigma(k) / FOUR
                        call lyp_c(epsc, ec_rhoa, ec_rhob, ec_sigaa, ec_sigab, ec_sigbb, rho_a, rho_b, sigma_aa, sigma_ab, sigma_bb)
                        eps(k) = xfrac * sr_eps + epsc
                        vrho(k) = xfrac * sr_rho + ec_rhoa
                        vsigma(k) = xfrac * sr_sigma + ec_sigaa * (ONE/TWO) + ec_sigab * (ONE/FOUR)
                  else
                        eps(k) = ZERO
                        vrho(k) = ZERO
                        vsigma(k) = ZERO
                  end if
            end do
      end subroutine hjs_blyp_xc
      

      subroutine hjs_b88_xonly(eps, vrho, vsigma, rho, sigma, npt, srexx, omega)
            real(F64), dimension(:), intent(out) :: eps
            real(F64), dimension(:), intent(out) :: vrho
            real(F64), dimension(:), intent(out) :: vsigma
            real(F64), dimension(:), intent(in)  :: rho
            real(F64), dimension(:), intent(in)  :: sigma
            integer, intent(in)                  :: npt
            real(F64), intent(in)                :: srexx
            real(F64), intent(in)                :: omega

            integer :: k
            real(F64) :: sr_eps, sr_rho, sr_sigma, xfrac
            real(F64) :: rho_a, rho_b, sigma_aa, sigma_ab, sigma_bb

            xfrac = ONE - srexx
            do k = 1, npt
                  if (rho(k) > HJS_X_RHO_THRESH) then
                        call hjs_sr_b88_x(sr_eps, sr_rho, sr_sigma, rho(k), sigma(k), omega)
                        rho_a = rho(k) / TWO
                        rho_b = rho(k) / TWO
                        sigma_aa = sigma(k) / FOUR
                        sigma_ab = sigma(k) / FOUR
                        sigma_bb = sigma(k) / FOUR
                        eps(k) = xfrac * sr_eps
                        vrho(k) = xfrac * sr_rho
                        vsigma(k) = xfrac * sr_sigma
                  else
                        eps(k) = ZERO
                        vrho(k) = ZERO
                        vsigma(k) = ZERO
                  end if
            end do
      end subroutine hjs_b88_xonly


      subroutine hjs_pbe_xonly(eps, vrho, vsigma, rho, sigma, npt, srexx, omega)
            real(F64), dimension(:), intent(out) :: eps
            real(F64), dimension(:), intent(out) :: vrho
            real(F64), dimension(:), intent(out) :: vsigma
            real(F64), dimension(:), intent(in)  :: rho
            real(F64), dimension(:), intent(in)  :: sigma
            integer, intent(in)                  :: npt
            real(F64), intent(in)                :: srexx
            real(F64), intent(in)                :: omega

            integer :: k
            real(F64) :: sr_eps, sr_rho, sr_sigma, xfrac
            real(F64) :: rho_a, rho_b, sigma_aa, sigma_ab, sigma_bb

            xfrac = ONE - srexx
            do k = 1, npt
                  if (rho(k) > HJS_X_RHO_THRESH) then
                        call hjs_sr_pbe_x(sr_eps, sr_rho, sr_sigma, rho(k), sigma(k), omega)
                        rho_a = rho(k) / TWO
                        rho_b = rho(k) / TWO
                        sigma_aa = sigma(k) / FOUR
                        sigma_ab = sigma(k) / FOUR
                        sigma_bb = sigma(k) / FOUR
                        eps(k) = xfrac * sr_eps
                        vrho(k) = xfrac * sr_rho
                        vsigma(k) = xfrac * sr_sigma
                  else
                        eps(k) = ZERO
                        vrho(k) = ZERO
                        vsigma(k) = ZERO
                  end if
            end do
      end subroutine hjs_pbe_xonly


      subroutine u_hjs_blyp_xc(eps, vrho, vsigma, rho, sigma, npt, srexx, omega)
            real(F64), dimension(:), intent(out)    :: eps
            real(F64), dimension(:, :), intent(out) :: vrho
            real(F64), dimension(:, :), intent(out) :: vsigma
            real(F64), dimension(:, :), intent(in)  :: rho
            real(F64), dimension(:, :), intent(in)  :: sigma
            integer, intent(in)                     :: npt
            real(F64), intent(in)                   :: srexx
            real(F64), intent(in)                   :: omega

            integer :: k
            real(F64) :: sr_eps, sr_rho, sr_sigma, xfrac
            real(F64) :: epsc, ec_rhoa, ec_rhob, ec_sigaa, ec_sigab, ec_sigbb

            xfrac = ONE - srexx
            do k = 1, npt
                  eps(k) = ZERO
                  vrho(1, k) = ZERO
                  vrho(2, k) = ZERO
                  vsigma(1, k) = ZERO
                  vsigma(2, k) = ZERO
                  vsigma(3, k) = ZERO
                  
                  if (TWO*rho(1, k) > HJS_X_RHO_THRESH .and. TWO*rho(2, k) > HJS_X_RHO_THRESH) then
                        call lyp_c(epsc, ec_rhoa, ec_rhob, ec_sigaa, ec_sigab, ec_sigbb, rho(1, k), rho(2, k), &
                              sigma(1, k), sigma(2, k), sigma(3, k))
                        eps(k) = eps(k) + epsc
                        vrho(1, k) = vrho(1, k) + ec_rhoa
                        vrho(2, k) = vrho(2, k) + ec_rhob
                        vsigma(1, k) = vsigma(1, k) + ec_sigaa
                        vsigma(2, k) = vsigma(2, k) + ec_sigab
                        vsigma(3, k) = vsigma(3, k) + ec_sigbb
                  end if

                  if (TWO*rho(1, k) > HJS_X_RHO_THRESH) then
                        call hjs_sr_b88_x(sr_eps, sr_rho, sr_sigma, TWO*rho(1, k), FOUR*sigma(1, k), omega)
                        eps(k) = eps(k) + xfrac * (rho(1, k) / (rho(1, k) + rho(2, k))) * sr_eps
                        vrho(1, k) = vrho(1, k) + xfrac * sr_rho
                        vsigma(1, k) = vsigma(1, k) + TWO * xfrac * sr_sigma
                  end if
                  
                  if (TWO*rho(2, k) > HJS_X_RHO_THRESH) then
                        call hjs_sr_b88_x(sr_eps, sr_rho, sr_sigma, TWO*rho(2, k), FOUR*sigma(3, k), omega)
                        eps(k) = eps(k) + xfrac * (rho(2, k) / (rho(1, k) + rho(2, k))) * sr_eps
                        vrho(2, k) = vrho(2, k) + xfrac * sr_rho
                        vsigma(3, k) = vsigma(3, k) + TWO * xfrac * sr_sigma
                  end if
            end do
      end subroutine u_hjs_blyp_xc


      subroutine u_hjs_b88_xonly(eps, vrho, vsigma, rho, sigma, npt, srexx, omega)
            real(F64), dimension(:), intent(out)    :: eps
            real(F64), dimension(:, :), intent(out) :: vrho
            real(F64), dimension(:, :), intent(out) :: vsigma
            real(F64), dimension(:, :), intent(in)  :: rho
            real(F64), dimension(:, :), intent(in)  :: sigma
            integer, intent(in)                     :: npt
            real(F64), intent(in)                   :: srexx
            real(F64), intent(in)                   :: omega

            integer :: k
            real(F64) :: sr_eps, sr_rho, sr_sigma, xfrac

            xfrac = ONE - srexx
            do k = 1, npt
                  eps(k) = ZERO
                  vrho(1, k) = ZERO
                  vrho(2, k) = ZERO
                  vsigma(1, k) = ZERO
                  vsigma(2, k) = ZERO
                  vsigma(3, k) = ZERO
                  
                  if (TWO*rho(1, k) > HJS_X_RHO_THRESH) then
                        call hjs_sr_b88_x(sr_eps, sr_rho, sr_sigma, TWO*rho(1, k), FOUR*sigma(1, k), omega)
                        eps(k) = eps(k) + xfrac * (rho(1, k) / (rho(1, k) + rho(2, k))) * sr_eps
                        vrho(1, k) = vrho(1, k) + xfrac * sr_rho
                        vsigma(1, k) = vsigma(1, k) + TWO * xfrac * sr_sigma
                  end if
                  
                  if (TWO*rho(2, k) > HJS_X_RHO_THRESH) then
                        call hjs_sr_b88_x(sr_eps, sr_rho, sr_sigma, TWO*rho(2, k), FOUR*sigma(3, k), omega)
                        eps(k) = eps(k) + xfrac * (rho(2, k) / (rho(1, k) + rho(2, k))) * sr_eps
                        vrho(2, k) = vrho(2, k) + xfrac * sr_rho
                        vsigma(3, k) = vsigma(3, k) + TWO * xfrac * sr_sigma
                  end if
            end do
      end subroutine u_hjs_b88_xonly


      subroutine u_hjs_pbe_xonly(eps, vrho, vsigma, rho, sigma, npt, srexx, omega)
            real(F64), dimension(:), intent(out)    :: eps
            real(F64), dimension(:, :), intent(out) :: vrho
            real(F64), dimension(:, :), intent(out) :: vsigma
            real(F64), dimension(:, :), intent(in)  :: rho
            real(F64), dimension(:, :), intent(in)  :: sigma
            integer, intent(in)                     :: npt
            real(F64), intent(in)                   :: srexx
            real(F64), intent(in)                   :: omega

            integer :: k
            real(F64) :: sr_eps, sr_rho, sr_sigma, xfrac

            xfrac = ONE - srexx
            do k = 1, npt
                  eps(k) = ZERO
                  vrho(1, k) = ZERO
                  vrho(2, k) = ZERO
                  vsigma(1, k) = ZERO
                  vsigma(2, k) = ZERO
                  vsigma(3, k) = ZERO
                  
                  if (TWO*rho(1, k) > HJS_X_RHO_THRESH) then
                        call hjs_sr_pbe_x(sr_eps, sr_rho, sr_sigma, TWO*rho(1, k), FOUR*sigma(1, k), omega)
                        eps(k) = eps(k) + xfrac * (rho(1, k) / (rho(1, k) + rho(2, k))) * sr_eps
                        vrho(1, k) = vrho(1, k) + xfrac * sr_rho
                        vsigma(1, k) = vsigma(1, k) + TWO * xfrac * sr_sigma
                  end if
                  
                  if (TWO*rho(2, k) > HJS_X_RHO_THRESH) then
                        call hjs_sr_pbe_x(sr_eps, sr_rho, sr_sigma, TWO*rho(2, k), FOUR*sigma(3, k), omega)
                        eps(k) = eps(k) + xfrac * (rho(2, k) / (rho(1, k) + rho(2, k))) * sr_eps
                        vrho(2, k) = vrho(2, k) + xfrac * sr_rho
                        vsigma(3, k) = vsigma(3, k) + TWO * xfrac * sr_sigma
                  end if
            end do
      end subroutine u_hjs_pbe_xonly
end module hjs_xc_energy
