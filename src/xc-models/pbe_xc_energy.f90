module pbe_xc_energy
      use arithmetic
      use math_constants
      use pbe_x_energy
      use pbe_c_energy

      implicit none

contains

      subroutine pbe_xc(eps, e_rho, e_sigma, rho, sigma, npt, exx)
            !
            ! Compute the PBE exchange-correlation energy for closed shell densities.
            !
            real(F64), dimension(:), intent(out) :: eps
            real(F64), dimension(:), intent(out) :: e_rho
            real(F64), dimension(:), intent(out) :: e_sigma
            real(F64), dimension(:), intent(in)  :: rho
            real(F64), dimension(:), intent(in)  :: sigma
            integer, intent(in)                  :: npt
            real(F64), intent(in)                :: exx

            integer :: k
            real(F64) :: epsx, ex_rho, ex_sigma, xfrac
            real(F64) :: epsc, ec_rho, ec_sigma
            
            xfrac = ONE - exx
            do k = 1, npt
                  if (rho(k) > PBE_X_RHO_THRESH) then
                        call pbe_x(epsx, ex_rho, ex_sigma, rho(k), sigma(k))
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
      end subroutine pbe_xc


      subroutine pbe_xonly(eps, e_rho, e_sigma, rho, sigma, npt, exx)
            !
            ! Compute the PBE exchange energy for closed shell densities.
            !
            real(F64), dimension(:), intent(out) :: eps
            real(F64), dimension(:), intent(out) :: e_rho
            real(F64), dimension(:), intent(out) :: e_sigma
            real(F64), dimension(:), intent(in)  :: rho
            real(F64), dimension(:), intent(in)  :: sigma
            integer, intent(in)                  :: npt
            real(F64), intent(in)                :: exx

            integer :: k
            real(F64) :: epsx, ex_rho, ex_sigma, xfrac
            
            xfrac = ONE - exx
            do k = 1, npt
                  if (rho(k) > PBE_X_RHO_THRESH) then
                        call pbe_x(epsx, ex_rho, ex_sigma, rho(k), sigma(k))
                        eps(k) = xfrac * epsx
                        e_rho(k) = xfrac * ex_rho
                        e_sigma(k) = xfrac * ex_sigma
                  else
                        eps(k) = ZERO
                        e_rho(k) = ZERO
                        e_sigma(k) = ZERO
                  end if
            end do
      end subroutine pbe_xonly


      subroutine ec_pbe_xc(eps, vrho, vsigma, vlapl, vtau, rho, sigma, lapl, tau, npt, srexx, omega)
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
                  if (rho(k) > PBE_X_RHO_THRESH) then
                        call ec_sr_pbe_x(sr_eps, sr_rho, sr_sigma, sr_lapl, sr_tau, rho(k), sigma(k), lapl(k), tau(k), omega)
                        call pbe_c(epsc, ec_rho, ec_sigma, rho(k), sigma(k))
                        eps(k) = xfrac * sr_eps + epsc
                        vrho(k) = xfrac * sr_rho + ec_rho
                        vsigma(k) = xfrac * sr_sigma + ec_sigma
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
      end subroutine ec_pbe_xc


      subroutine ec_pbesol_xc(eps, vrho, vsigma, vlapl, vtau, rho, sigma, lapl, tau, npt, srexx, omega)
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
                  if (rho(k) > PBE_X_RHO_THRESH) then
                        call ec_sr_pbesol_x(sr_eps, sr_rho, sr_sigma, sr_lapl, sr_tau, rho(k), sigma(k), lapl(k), tau(k), omega)
                        call pbesol_c(epsc, ec_rho, ec_sigma, rho(k), sigma(k))
                        eps(k) = xfrac * sr_eps + epsc
                        vrho(k) = xfrac * sr_rho + ec_rho
                        vsigma(k) = xfrac * sr_sigma + ec_sigma
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
      end subroutine ec_pbesol_xc


      subroutine ec_pbe_xonly(eps, vrho, vsigma, vlapl, vtau, rho, sigma, lapl, tau, npt, srexx, omega)
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
                  if (rho(k) > PBE_X_RHO_THRESH) then
                        call ec_sr_pbe_x(sr_eps, sr_rho, sr_sigma, sr_lapl, sr_tau, rho(k), sigma(k), lapl(k), tau(k), omega)
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
      end subroutine ec_pbe_xonly
      
      
      subroutine u_pbe_xc(eps, e_rho, e_sigma, rho, sigma, npt, exx)
            !
            ! Compute the PBE exchange-correlation energy for open shell densities.
            !
            real(F64), dimension(:), intent(out)    :: eps
            real(F64), dimension(:, :), intent(out) :: e_rho
            real(F64), dimension(:, :), intent(out) :: e_sigma
            real(F64), dimension(:, :), intent(in)  :: rho
            real(F64), dimension(:, :), intent(in)  :: sigma
            integer, intent(in)                     :: npt
            real(F64), intent(in)                   :: exx

            integer :: k
            real(F64) :: epsx, ex_rho, ex_sigma, xfrac
            real(F64) :: epsc, ec_rhoa, ec_rhob, ec_sigaa, ec_sigab, ec_sigbb

            xfrac = ONE - exx
            do k = 1, npt
                  call u_pbe_c(epsc, ec_rhoa, ec_rhob, ec_sigaa, ec_sigab, ec_sigbb, rho(1, k), rho(2, k), &
                        sigma(1, k), sigma(2, k), sigma(3, k))
                  eps(k) = epsc
                  e_rho(1, k) = ec_rhoa
                  e_rho(2, k) = ec_rhob
                  e_sigma(1, k) = ec_sigaa
                  e_sigma(2, k) = ec_sigab
                  e_sigma(3, k) = ec_sigbb

                  if (TWO*rho(1, k) > PBE_X_RHO_THRESH) then
                        call pbe_x(epsx, ex_rho, ex_sigma, TWO*rho(1, k), FOUR*sigma(1, k))
                        eps(k) = eps(k) + xfrac * (rho(1, k) / (rho(1, k) + rho(2, k))) * epsx
                        e_rho(1, k) = e_rho(1, k) + xfrac * ex_rho
                        e_sigma(1, k) = e_sigma(1, k) + TWO * xfrac * ex_sigma
                  end if

                  if (TWO*rho(2, k) > PBE_X_RHO_THRESH) then
                        call pbe_x(epsx, ex_rho, ex_sigma, TWO*rho(2, k), FOUR*sigma(3, k))
                        eps(k) = eps(k) + xfrac * (rho(2, k) / (rho(1, k) + rho(2, k))) * epsx
                        e_rho(2, k) = e_rho(2, k) + xfrac * ex_rho
                        e_sigma(3, k) = e_sigma(3, k) + TWO * xfrac * ex_sigma
                  end if         
            end do
      end subroutine u_pbe_xc


      subroutine u_pbe_xonly(eps, e_rho, e_sigma, rho, sigma, npt, exx)
            !
            ! Compute the PBE exchange energy for open shell densities.
            !
            real(F64), dimension(:), intent(out)    :: eps
            real(F64), dimension(:, :), intent(out) :: e_rho
            real(F64), dimension(:, :), intent(out) :: e_sigma
            real(F64), dimension(:, :), intent(in)  :: rho
            real(F64), dimension(:, :), intent(in)  :: sigma
            integer, intent(in)                     :: npt
            real(F64), intent(in)                   :: exx

            integer :: k
            real(F64) :: epsx, ex_rho, ex_sigma, xfrac

            xfrac = ONE - exx
            do k = 1, npt
                  eps(k) = ZERO
                  e_rho(1, k) = ZERO
                  e_rho(2, k) = ZERO
                  e_sigma(1, k) = ZERO
                  e_sigma(2, k) = ZERO
                  e_sigma(3, k) = ZERO

                  if (TWO*rho(1, k) > PBE_X_RHO_THRESH) then
                        call pbe_x(epsx, ex_rho, ex_sigma, TWO*rho(1, k), FOUR*sigma(1, k))
                        eps(k) = eps(k) + xfrac * (rho(1, k) / (rho(1, k) + rho(2, k))) * epsx
                        e_rho(1, k) = e_rho(1, k) + xfrac * ex_rho
                        e_sigma(1, k) = e_sigma(1, k) + TWO * xfrac * ex_sigma
                  end if

                  if (TWO*rho(2, k) > PBE_X_RHO_THRESH) then
                        call pbe_x(epsx, ex_rho, ex_sigma, TWO*rho(2, k), FOUR*sigma(3, k))
                        eps(k) = eps(k) + xfrac * (rho(2, k) / (rho(1, k) + rho(2, k))) * epsx
                        e_rho(2, k) = e_rho(2, k) + xfrac * ex_rho
                        e_sigma(3, k) = e_sigma(3, k) + TWO * xfrac * ex_sigma
                  end if         
            end do
      end subroutine u_pbe_xonly


      subroutine pbesol_xc(eps, e_rho, e_sigma, rho, sigma, npt, exx)
            !
            ! Compute the PBEsol exchange-correlation energy for closed shell densities.
            !
            real(F64), dimension(:), intent(out) :: eps
            real(F64), dimension(:), intent(out) :: e_rho
            real(F64), dimension(:), intent(out) :: e_sigma
            real(F64), dimension(:), intent(in)  :: rho
            real(F64), dimension(:), intent(in)  :: sigma
            integer, intent(in)                  :: npt
            real(F64), intent(in)                :: exx

            integer :: k
            real(F64) :: epsx, ex_rho, ex_sigma, xfrac
            real(F64) :: epsc, ec_rho, ec_sigma
            
            xfrac = ONE - exx
            do k = 1, npt
                  if (rho(k) > PBE_X_RHO_THRESH) then
                        call pbesol_x(epsx, ex_rho, ex_sigma, rho(k), sigma(k))
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
      end subroutine pbesol_xc


      subroutine u_pbesol_xc(eps, e_rho, e_sigma, rho, sigma, npt, exx)
            !
            ! Compute the PBEsol exchange-correlation energy for open shell densities.
            !
            real(F64), dimension(:), intent(out)    :: eps
            real(F64), dimension(:, :), intent(out) :: e_rho
            real(F64), dimension(:, :), intent(out) :: e_sigma
            real(F64), dimension(:, :), intent(in)  :: rho
            real(F64), dimension(:, :), intent(in)  :: sigma
            integer, intent(in)                     :: npt
            real(F64), intent(in)                   :: exx

            integer :: k
            real(F64) :: epsx, ex_rho, ex_sigma, xfrac
            real(F64) :: epsc, ec_rhoa, ec_rhob, ec_sigaa, ec_sigab, ec_sigbb

            xfrac = ONE - exx
            do k = 1, npt
                  call u_pbesol_c(epsc, ec_rhoa, ec_rhob, ec_sigaa, ec_sigab, ec_sigbb, rho(1, k), rho(2, k), &
                        sigma(1, k), sigma(2, k), sigma(3, k))
                  eps(k) = epsc
                  e_rho(1, k) = ec_rhoa
                  e_rho(2, k) = ec_rhob
                  e_sigma(1, k) = ec_sigaa
                  e_sigma(2, k) = ec_sigab
                  e_sigma(3, k) = ec_sigbb

                  if (TWO*rho(1, k) > PBE_X_RHO_THRESH) then
                        call pbesol_x(epsx, ex_rho, ex_sigma, TWO*rho(1, k), FOUR*sigma(1, k))
                        eps(k) = eps(k) + xfrac * (rho(1, k) / (rho(1, k) + rho(2, k))) * epsx
                        e_rho(1, k) = e_rho(1, k) + xfrac * ex_rho
                        e_sigma(1, k) = e_sigma(1, k) + TWO * xfrac * ex_sigma
                  end if

                  if (TWO*rho(2, k) > PBE_X_RHO_THRESH) then
                        call pbesol_x(epsx, ex_rho, ex_sigma, TWO*rho(2, k), FOUR*sigma(3, k))
                        eps(k) = eps(k) + xfrac * (rho(2, k) / (rho(1, k) + rho(2, k))) * epsx
                        e_rho(2, k) = e_rho(2, k) + xfrac * ex_rho
                        e_sigma(3, k) = e_sigma(3, k) + TWO * xfrac * ex_sigma
                  end if                  
            end do
      end subroutine u_pbesol_xc

      
      subroutine u_ec_pbe_xc(eps, vrho, vsigma, vlapl, vtau, rho, sigma, lapl, tau, npt, srexx, omega)
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
                  call u_pbe_c(epsc, ec_rhoa, ec_rhob, ec_sigaa, ec_sigab, ec_sigbb, rho(1, k), rho(2, k), &
                        sigma(1, k), sigma(2, k), sigma(3, k))
                  eps(k) = epsc
                  vrho(1, k) = ec_rhoa
                  vrho(2, k) = ec_rhob
                  vsigma(1, k) = ec_sigaa
                  vsigma(2, k) = ec_sigab
                  vsigma(3, k) = ec_sigbb
                  vtau(1, k) = ZERO
                  vtau(2, k) = ZERO
                  vlapl(1, k) = ZERO
                  vlapl(2, k) = ZERO

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
      end subroutine u_ec_pbe_xc


      subroutine u_ec_pbesol_xc(eps, vrho, vsigma, vlapl, vtau, rho, sigma, lapl, tau, npt, srexx, omega)
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
                  call u_pbesol_c(epsc, ec_rhoa, ec_rhob, ec_sigaa, ec_sigab, ec_sigbb, rho(1, k), rho(2, k), &
                        sigma(1, k), sigma(2, k), sigma(3, k))
                  eps(k) = epsc
                  vrho(1, k) = ec_rhoa
                  vrho(2, k) = ec_rhob
                  vsigma(1, k) = ec_sigaa
                  vsigma(2, k) = ec_sigab
                  vsigma(3, k) = ec_sigbb
                  vtau(1, k) = ZERO
                  vtau(2, k) = ZERO
                  vlapl(1, k) = ZERO
                  vlapl(2, k) = ZERO

                  if (TWO*rho(1, k) > PBE_X_RHO_THRESH) then
                        call ec_sr_pbesol_x(sr_eps, sr_rho, sr_sigma, sr_lapl, sr_tau, TWO*rho(1, k), FOUR*sigma(1, k), &
                              TWO*lapl(1, k), TWO*tau(1, k), omega)
                        eps(k) = eps(k) + xfrac * (rho(1, k) / (rho(1, k) + rho(2, k))) * sr_eps
                        vrho(1, k) = vrho(1, k) + xfrac * sr_rho
                        vsigma(1, k) = vsigma(1, k) + TWO * xfrac * sr_sigma
                        vlapl(1, k) = vlapl(1, k) + xfrac * sr_lapl
                        vtau(1, k) = vtau(1, k) + xfrac * sr_tau
                  end if
                  
                  if (TWO*rho(2, k) > PBE_X_RHO_THRESH) then
                        call ec_sr_pbesol_x(sr_eps, sr_rho, sr_sigma, sr_lapl, sr_tau, TWO*rho(2, k), FOUR*sigma(3, k), &
                              TWO*lapl(2, k), TWO*tau(2, k), omega)
                        eps(k) = eps(k) + xfrac * (rho(2, k) / (rho(1, k) + rho(2, k))) * sr_eps
                        vrho(2, k) = vrho(2, k) + xfrac * sr_rho
                        vsigma(3, k) = vsigma(3, k) + TWO * xfrac * sr_sigma
                        vlapl(2, k) = vlapl(2, k) + xfrac * sr_lapl
                        vtau(2, k) = vtau(2, k) + xfrac * sr_tau
                  end if
            end do
      end subroutine u_ec_pbesol_xc


      subroutine u_ec_pbe_xonly(eps, vrho, vsigma, vlapl, vtau, rho, sigma, lapl, tau, npt, srexx, omega)
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
      end subroutine u_ec_pbe_xonly


      subroutine ec_sh_pbe_xc(eps, vrho, vsigma, vlapl, vtau, rho, sigma, lapl, tau, npt, srexx, omega)
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
            real(F64) :: epsc, ec_rho, ec_sigma

            srfrac = ONE - srexx
            do k = 1, npt
                  if (rho(k) > PBE_X_RHO_THRESH) then
                        call ec_sh_pbe_x(epsx, ex_rho, ex_sigma, ex_lapl, ex_tau, rho(k), &
                              sigma(k), lapl(k), tau(k), srfrac, omega)
                        call pbe_c(epsc, ec_rho, ec_sigma, rho(k), sigma(k))
                        eps(k) = epsx + epsc
                        vrho(k) = ex_rho + ec_rho
                        vsigma(k) = ex_sigma + ec_sigma
                        vlapl(k) = ex_lapl
                        vtau(k) = ex_tau
                  else
                        eps(k) = ZERO
                        vrho(k) = ZERO
                        vsigma(k) = ZERO
                        vlapl(k) = ZERO
                        vtau(k) = ZERO
                  end if
            end do
      end subroutine ec_sh_pbe_xc

      
      subroutine u_ec_sh_pbe_xc(eps, vrho, vsigma, vlapl, vtau, rho, sigma, lapl, tau, npt, srexx, omega)
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
            real(F64) :: epsc, ec_rhoa, ec_rhob, ec_sigaa, ec_sigab, ec_sigbb

            srfrac = ONE - srexx
            do k = 1, npt
                  call u_pbe_c(epsc, ec_rhoa, ec_rhob, ec_sigaa, ec_sigab, ec_sigbb, rho(1, k), rho(2, k), &
                        sigma(1, k), sigma(2, k), sigma(3, k))
                  eps(k) = epsc
                  vrho(1, k) = ec_rhoa
                  vrho(2, k) = ec_rhob
                  vsigma(1, k) = ec_sigaa
                  vsigma(2, k) = ec_sigab
                  vsigma(3, k) = ec_sigbb
                  vtau(1, k) = ZERO
                  vtau(2, k) = ZERO
                  vlapl(1, k) = ZERO
                  vlapl(2, k) = ZERO

                  if (TWO*rho(1, k) > PBE_X_RHO_THRESH) then
                        call ec_sh_pbe_x(epsx, ex_rho, ex_sigma, ex_lapl, ex_tau, TWO*rho(1, k), FOUR*sigma(1, k), &
                              TWO*lapl(1, k), TWO*tau(1, k), srfrac, omega)
                        eps(k) = eps(k) + (rho(1, k) / (rho(1, k) + rho(2, k))) * epsx
                        vrho(1, k) = vrho(1, k) + ex_rho
                        vsigma(1, k) = vsigma(1, k) + TWO * ex_sigma
                        vlapl(1, k) = vlapl(1, k) + ex_lapl
                        vtau(1, k) = vtau(1, k) + ex_tau
                  end if
                  
                  if (TWO*rho(2, k) > PBE_X_RHO_THRESH) then
                        call ec_sh_pbe_x(epsx, ex_rho, ex_sigma, ex_lapl, ex_tau, TWO*rho(2, k), FOUR*sigma(3, k), &
                              TWO*lapl(2, k), TWO*tau(2, k), srfrac, omega)
                        eps(k) = eps(k) + (rho(2, k) / (rho(1, k) + rho(2, k))) * epsx
                        vrho(2, k) = vrho(2, k) + ex_rho
                        vsigma(3, k) = vsigma(3, k) + TWO * ex_sigma
                        vlapl(2, k) = vlapl(2, k) + ex_lapl
                        vtau(2, k) = vtau(2, k) + ex_tau
                  end if
            end do
      end subroutine u_ec_sh_pbe_xc
end module pbe_xc_energy
