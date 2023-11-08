module pw86_xc_energy
      use pbe_c_energy
      use pw86_x_energy

contains

      subroutine pw86pbe_xc(eps, e_rho, e_sigma, rho, sigma, npt, exx)
            !
            ! Compute the PW86-PBE exchange-correlation energy for closed shell densities.
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
                  if (rho(k) > PW86_X_RHO_THRESH) then
                        call pw86_x(epsx, ex_rho, ex_sigma, rho(k), sigma(k))
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
      end subroutine pw86pbe_xc


      subroutine rpw86pbe_xc(eps, e_rho, e_sigma, rho, sigma, npt, exx)
            !
            ! Compute the RPW86-PBE exchange-correlation energy for closed shell densities.
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
                  if (rho(k) > PW86_X_RHO_THRESH) then
                        call rpw86_x(epsx, ex_rho, ex_sigma, rho(k), sigma(k))
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
      end subroutine rpw86pbe_xc


      subroutine u_pw86pbe_xc(eps, e_rho, e_sigma, rho, sigma, npt, exx)
            !
            ! Compute the PW86-PBE exchange-correlation energy for open shell densities.
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

                  if (TWO*rho(1, k) > PW86_X_RHO_THRESH) then
                        call pw86_x(epsx, ex_rho, ex_sigma, TWO*rho(1, k), FOUR*sigma(1, k))
                        eps(k) = eps(k) + xfrac * (rho(1, k) / (rho(1, k) + rho(2, k))) * epsx
                        e_rho(1, k) = e_rho(1, k) + xfrac * ex_rho
                        e_sigma(1, k) = e_sigma(1, k) + TWO * xfrac * ex_sigma
                  end if

                  if (TWO*rho(2, k) > PW86_X_RHO_THRESH) then
                        call pw86_x(epsx, ex_rho, ex_sigma, TWO*rho(2, k), FOUR*sigma(3, k))
                        eps(k) = eps(k) + xfrac * (rho(2, k) / (rho(1, k) + rho(2, k))) * epsx
                        e_rho(2, k) = e_rho(2, k) + xfrac * ex_rho
                        e_sigma(3, k) = e_sigma(3, k) + TWO * xfrac * ex_sigma
                  end if         
            end do
      end subroutine u_pw86pbe_xc


      subroutine u_rpw86pbe_xc(eps, e_rho, e_sigma, rho, sigma, npt, exx)
            !
            ! Compute the RPW86-PBE exchange-correlation energy for open shell densities.
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

                  if (TWO*rho(1, k) > PW86_X_RHO_THRESH) then
                        call rpw86_x(epsx, ex_rho, ex_sigma, TWO*rho(1, k), FOUR*sigma(1, k))
                        eps(k) = eps(k) + xfrac * (rho(1, k) / (rho(1, k) + rho(2, k))) * epsx
                        e_rho(1, k) = e_rho(1, k) + xfrac * ex_rho
                        e_sigma(1, k) = e_sigma(1, k) + TWO * xfrac * ex_sigma
                  end if

                  if (TWO*rho(2, k) > PW86_X_RHO_THRESH) then
                        call rpw86_x(epsx, ex_rho, ex_sigma, TWO*rho(2, k), FOUR*sigma(3, k))
                        eps(k) = eps(k) + xfrac * (rho(2, k) / (rho(1, k) + rho(2, k))) * epsx
                        e_rho(2, k) = e_rho(2, k) + xfrac * ex_rho
                        e_sigma(3, k) = e_sigma(3, k) + TWO * xfrac * ex_sigma
                  end if         
            end do
      end subroutine u_rpw86pbe_xc
end module pw86_xc_energy
