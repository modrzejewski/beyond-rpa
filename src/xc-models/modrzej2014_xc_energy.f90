module modrzej2014_xc_energy
      use arithmetic
      use math_constants
      use hjs_x_energy
      use modrzej2012_c_energy
      !
      ! MCS and MCSh exchange-correlation functionals 
      ! Modrzejewski, M., Chałasiński, G., Szczęśniak, M.M., 
      ! J. Chem. Theory Comput. 10, 4297 (2014); doi: 10.1021/ct500707w
      ! 
      ! Parameter G of the MCS (MCSh) correlation component
      !
      real(F64), parameter :: MCS_GPARAM = 0.075_F64
      real(F64), parameter :: MCSH_GPARAM = 0.100_F64
      
contains

      subroutine mcs_xc(eps, vrho, vsigma, vtau, rho, sigma, tau, npt, srexx, omega)
            !
            ! Semilocal part of the MCS-D3 and MCS-MBD functionals
            !
            ! 1. Modrzejewski, M., Chałasiński, G., Szczęśniak, M.M., 
            !    J. Chem. Theory Comput. 10, 4297 (2014); doi: 10.1021/ct500707w
            !
            real(F64), dimension(:), intent(out) :: eps
            real(F64), dimension(:), intent(out) :: vrho
            real(F64), dimension(:), intent(out) :: vsigma
            real(F64), dimension(:), intent(out) :: vtau
            real(F64), dimension(:), intent(in)  :: rho
            real(F64), dimension(:), intent(in)  :: sigma
            real(F64), dimension(:), intent(in)  :: tau
            integer, intent(in)                  :: npt
            real(F64), intent(in)                :: srexx
            real(F64), intent(in)                :: omega

            call wpbesol_mlrcs_template(rho, sigma, tau, eps, vrho, vsigma, vtau, npt, MCS_GPARAM, srexx, omega)
      end subroutine mcs_xc


      subroutine mcsh_xc(eps, vrho, vsigma, vtau, rho, sigma, tau, npt, srexx, omega)
            !
            ! Semilocal part of the MCSh-D3 and MCSh-MBD functionals
            !
            ! 1. Modrzejewski, M., Chałasiński, G., Szczęśniak, M.M., 
            !    J. Chem. Theory Comput. 10, 4297 (2014); doi: 10.1021/ct500707w
            !
            real(F64), dimension(:), intent(out) :: eps
            real(F64), dimension(:), intent(out) :: vrho
            real(F64), dimension(:), intent(out) :: vsigma
            real(F64), dimension(:), intent(out) :: vtau
            real(F64), dimension(:), intent(in)  :: rho
            real(F64), dimension(:), intent(in)  :: sigma
            real(F64), dimension(:), intent(in)  :: tau
            integer, intent(in)                  :: npt
            real(F64), intent(in)                :: srexx
            real(F64), intent(in)                :: omega

            call wpbesol_mlrcs_template(rho, sigma, tau, eps, vrho, vsigma, vtau, npt, MCSH_GPARAM, srexx, omega)
      end subroutine mcsh_xc


      subroutine wpbesol_mlrcs_template(rho, sigma, tau, eps, vrho, vsigma, vtau, npt, gparam, srexx, omega)
            !
            ! Meta-GGA correlation of Modrzejewski et al. + Henderson-Janesko-Scuseria wPBEsol
            ! range-separated exchange
            !
            real(F64), dimension(:), intent(in)  :: rho
            real(F64), dimension(:), intent(in)  :: sigma
            real(F64), dimension(:), intent(in)  :: tau
            real(F64), dimension(:), intent(out) :: eps
            real(F64), dimension(:), intent(out) :: vrho
            real(F64), dimension(:), intent(out) :: vsigma
            real(F64), dimension(:), intent(out) :: vtau
            integer, intent(in)                  :: npt
            real(F64), intent(in)                :: gparam
            real(F64), intent(in)                :: srexx
            real(F64), intent(in)                :: omega

            integer :: k
            real(F64) :: epsx, ex_rho, ex_sigma, xfrac

            xfrac = ONE - srexx
            do k = 1, npt
                  call mlrcs12_c(eps(k), vrho(k), vsigma(k), vtau(k), rho(k), sigma(k), &
                        tau(k), gparam)
                  if (rho(k) > HJS_X_RHO_THRESH) then
                        call hjs_sr_pbesol_x(epsx, ex_rho, ex_sigma, rho(k), sigma(k), omega)
                        eps(k) = eps(k) + xfrac * epsx
                        vrho(k) = vrho(k) + xfrac * ex_rho
                        vsigma(k) = vsigma(k) + xfrac * ex_sigma
                  end if
            end do
      end subroutine wpbesol_mlrcs_template


      subroutine u_mcs_xc(eps, vrho, vsigma, vtau, rho, sigma, tau, npt, srexx, omega)
            !
            ! Semilocal part of the MCS-D3 and MCS-MBD functionals
            !
            ! 1. Modrzejewski, M., Chałasiński, G., Szczęśniak, M.M., 
            !    J. Chem. Theory Comput. 10, 4297 (2014); doi: 10.1021/ct500707w
            !
            real(F64), dimension(:), intent(out)    :: eps
            real(F64), dimension(:, :), intent(out) :: vrho
            real(F64), dimension(:, :), intent(out) :: vsigma
            real(F64), dimension(:, :), intent(out) :: vtau
            real(F64), dimension(:, :), intent(in)  :: rho
            real(F64), dimension(:, :), intent(in)  :: sigma
            real(F64), dimension(:, :), intent(in)  :: tau
            integer, intent(in)                     :: npt
            real(F64), intent(in)                   :: srexx
            real(F64), intent(in)                   :: omega

            call u_wpbesol_mlrcs_template(rho, sigma, tau, eps, vrho, vsigma, vtau, npt, MCS_GPARAM, srexx, omega)
      end subroutine u_mcs_xc


      subroutine u_mcsh_xc(eps, vrho, vsigma, vtau, rho, sigma, tau, npt, srexx, omega)
            !
            ! Semilocal part of the MCSh-D3 and MCSh-MBD functionals
            !
            ! 1. Modrzejewski, M., Chałasiński, G., Szczęśniak, M.M., 
            !    J. Chem. Theory Comput. 10, 4297 (2014); doi: 10.1021/ct500707w
            !
            real(F64), dimension(:), intent(out)    :: eps
            real(F64), dimension(:, :), intent(out) :: vrho
            real(F64), dimension(:, :), intent(out) :: vsigma
            real(F64), dimension(:, :), intent(out) :: vtau
            real(F64), dimension(:, :), intent(in)  :: rho
            real(F64), dimension(:, :), intent(in)  :: sigma
            real(F64), dimension(:, :), intent(in)  :: tau
            integer, intent(in)                     :: npt
            real(F64), intent(in)                   :: srexx
            real(F64), intent(in)                   :: omega

            call u_wpbesol_mlrcs_template(rho, sigma, tau, eps, vrho, vsigma, vtau, npt, MCSH_GPARAM, srexx, omega)
      end subroutine u_mcsh_xc


      subroutine u_wpbesol_mlrcs_template(rho, sigma, tau, eps, vrho, vsigma, vtau, npt, gparam, srexx, omega)
            !
            ! Meta-GGA correlation + Henderson-Janesko-Scuseria wPBEsol range-separated exchange
            !
            real(F64), dimension(:, :), intent(in)  :: rho
            real(F64), dimension(:, :), intent(in)  :: sigma
            real(F64), dimension(:, :), intent(in)  :: tau
            real(F64), dimension(:), intent(out)    :: eps
            real(F64), dimension(:, :), intent(out) :: vrho
            real(F64), dimension(:, :), intent(out) :: vsigma
            real(F64), dimension(:, :), intent(out) :: vtau
            integer, intent(in)                     :: npt
            real(F64), intent(in)                   :: gparam
            real(F64), intent(in)                   :: srexx
            real(F64), intent(in)                   :: omega

            integer :: k
            real(F64) :: epsc, ec_rhoa, ec_rhob, ec_sigaa, ec_sigab, ec_sigbb, ec_taua, ec_taub
            real(F64) :: epsx, ex_rho, ex_sigma, xfrac

            xfrac = ONE - srexx
            do k = 1, npt
                  call u_mlrcs12_c(epsc, ec_rhoa, ec_rhob, ec_sigaa, ec_sigab, ec_sigbb, ec_taua, ec_taub, &
                        rho(1, k), rho(2, k), sigma(1, k), sigma(2, k), sigma(3, k), tau(1, k), tau(2, k), gparam)
                  eps(k) = epsc
                  vrho(1, k) = ec_rhoa
                  vrho(2, k) = ec_rhob
                  vsigma(1, k) = ec_sigaa
                  vsigma(2, k) = ec_sigab
                  vsigma(3, k) = ec_sigbb
                  vtau(1, k) = ec_taua
                  vtau(2, k) = ec_taub

                  if (TWO*rho(1, k) > HJS_X_RHO_THRESH) then
                        call hjs_sr_pbesol_x(epsx, ex_rho, ex_sigma, TWO*rho(1, k), FOUR*sigma(1, k), omega)
                        eps(k) = eps(k) + xfrac * (rho(1, k) / (rho(1, k) + rho(2, k))) * epsx
                        vrho(1, k) = vrho(1, k) + xfrac * ex_rho
                        vsigma(1, k) = vsigma(1, k) + TWO * xfrac * ex_sigma
                  end if

                  if (TWO*rho(2, k) > HJS_X_RHO_THRESH) then
                        call hjs_sr_pbesol_x(epsx, ex_rho, ex_sigma, TWO*rho(2, k), FOUR*sigma(3, k), omega)
                        eps(k) = eps(k) + xfrac * (rho(2, k) / (rho(1, k) + rho(2, k))) * epsx
                        vrho(2, k) = vrho(2, k) + xfrac * ex_rho
                        vsigma(3, k) = vsigma(3, k) + TWO * xfrac * ex_sigma
                  end if
            end do
      end subroutine u_wpbesol_mlrcs_template
end module modrzej2014_xc_energy
