module modrzej2016_xc_auxiliary
      use arithmetic
      use math_constants
      use display
      use string
      use b88_x_energy
      use pbe_x_energy
      use modrzej2016_c_energy
      use modrzej2016_x_energy

      implicit none

      real(F64), parameter :: MODRZEJ2016_RHO_THRESH = 1.0E-10_F64

contains

      subroutine modrzej2016_vc_curve_display(vc)
            real(F64), dimension(:), intent(in) :: vc

            integer :: k
            real(F64) :: lambda
            character(80) :: line
            character(15), parameter :: fmt = "(F4.2,3X,F15.6)"

            call msg("Vc(lambda) curve for lambda=0.0...1.0")
            write(line, "(A6,6X,A10)") "lambda", "Vc(lambda)"
            call msg("lambda       Vc(lambda)", underline=.true.)
            write(line, fmt) 0.00_F64, 0.00_F64
            call msg(line)
            do k = 1, 100
                  lambda = k * 0.01_F64
                  write(line, fmt) lambda, vc(k)
                  call msg(line)
            end do
            call midrule()
      end subroutine modrzej2016_vc_curve_display


      subroutine modrzej2016_vxc_inf(vxc_inf, rho_a, rho_b, sigma_aa, sigma_bb)
            real(F64), intent(out) :: vxc_inf
            real(F64), intent(in)  :: rho_a
            real(F64), intent(in)  :: rho_b
            real(F64), intent(in)  :: sigma_aa
            real(F64), intent(in)  :: sigma_bb
            
            real(F64) :: epsc, epsx_a, epsx_b, ex_rho, ex_sigma, ec_rhoa, ec_rhob, vc, vx
            
            if (rho_a > MODRZEJ2016_RHO_THRESH .and. rho_b > MODRZEJ2016_RHO_THRESH) then
                  call u_point_charge_c(epsc, ec_rhoa, ec_rhob, rho_a, rho_b)
                  vc = (rho_a + rho_b) * epsc
                  call pbe_x(epsx_a, ex_rho, ex_sigma, TWO*rho_a, FOUR*sigma_aa)
                  call pbe_x(epsx_b, ex_rho, ex_sigma, TWO*rho_b, FOUR*sigma_bb)
                  vx = rho_a * epsx_a + rho_b * epsx_b
                  vxc_inf = vc + vx
            else if (rho_a > MODRZEJ2016_RHO_THRESH) then
                  call u_point_charge_c(epsc, ec_rhoa, ec_rhob, rho_a, ZERO)
                  vc = rho_a * epsc
                  call pbe_x(epsx_a, ex_rho, ex_sigma, TWO*rho_a, FOUR*sigma_aa)
                  vx = rho_a * epsx_a
                  vxc_inf = vc + vx
            else if (rho_b > MODRZEJ2016_RHO_THRESH) then
                  call u_point_charge_c(epsc, ec_rhoa, ec_rhob, ZERO, rho_b)
                  vc = rho_b * epsc
                  call pbe_x(epsx_b, ex_rho, ex_sigma, TWO*rho_b, FOUR*sigma_bb)
                  vx = rho_b * epsx_b
                  vxc_inf = vc + vx
            else
                  vxc_inf = ZERO
            end if
      end subroutine modrzej2016_vxc_inf


      subroutine modrzej2012_vc_curve(vc, grid_weight, rho_a, rho_b, sigma_aa, sigma_ab, &
            sigma_bb, tau_a, tau_b, gpar, gopp)
            real(F64), dimension(:), intent(inout) :: vc
            real(F64), intent(in)                  :: grid_weight
            real(F64), intent(in)                  :: rho_a
            real(F64), intent(in)                  :: rho_b
            real(F64), intent(in)                  :: sigma_aa
            real(F64), intent(in)                  :: sigma_ab
            real(F64), intent(in)                  :: sigma_bb
            real(F64), intent(in)                  :: tau_a
            real(F64), intent(in)                  :: tau_b
            real(F64), intent(in)                  :: gpar
            real(F64), intent(in)                  :: gopp

            integer :: k
            real(F64) :: vc_aa, vc_ab, vc_bb, lambda

            do k = 1, 100
                  lambda = k * 0.01_F64
                  call modrzej2012_vc_lambda(vc_aa, vc_ab, vc_bb, rho_a, rho_b, sigma_aa, sigma_ab, &
                        sigma_bb, tau_a, tau_b, gpar, gopp, lambda)
                  vc(k) = vc(k) + grid_weight * (vc_aa + TWO * vc_ab + vc_bb)
            end do
      end subroutine modrzej2012_vc_curve


      subroutine modrzej2016_vc_curve(vc, grid_weight, rho_a, rho_b, sigma_aa, sigma_ab, &
            sigma_bb, tau_a, tau_b)
            real(F64), dimension(:), intent(inout) :: vc
            real(F64), intent(in)                  :: grid_weight
            real(F64), intent(in)                  :: rho_a
            real(F64), intent(in)                  :: rho_b
            real(F64), intent(in)                  :: sigma_aa
            real(F64), intent(in)                  :: sigma_ab
            real(F64), intent(in)                  :: sigma_bb
            real(F64), intent(in)                  :: tau_a
            real(F64), intent(in)                  :: tau_b

            integer :: k
            real(F64) :: vck, lambda

            do k = 1, 100
                  lambda = k * 0.01_F64
                  call modrzej2016_vc_lambda(vck, rho_a, rho_b, sigma_aa, sigma_ab, sigma_bb, tau_a, tau_b, lambda)
                  vc(k) = vc(k) + grid_weight * vck
            end do
      end subroutine modrzej2016_vc_curve


      subroutine modrzej2016_vc_lambda(vc, rho_a, rho_b, sigma_aa, sigma_ab, sigma_bb, tau_a, tau_b, lambda)
            !
            ! 1. Modrzejewski et al. unpublished
            ! 2. Teale, A.M., Coriani, S., Helgaker, T. J. Chem. Phys. 132, 164115 (2010);
            !    doi: 10.1063/1.3380834
            !
            real(F64), intent(out) :: vc
            real(F64), intent(in)  :: rho_a
            real(F64), intent(in)  :: rho_b
            real(F64), intent(in)  :: sigma_aa
            real(F64), intent(in)  :: sigma_ab
            real(F64), intent(in)  :: sigma_bb
            real(F64), intent(in)  :: tau_a
            real(F64), intent(in)  :: tau_b
            real(F64), intent(in)  :: lambda
            
            real(F64) :: vc0, epsc0, rho, epsc, epscinf, vcinf_rhoa, vcinf_rhob
            real(F64) :: vcab, vcab_rhoa, vcab_rhob, vcab_sigaa, vcab_sigab, vcab_sigbb
            real(F64) :: vcaa, vcaa_rhoa, vcaa_sigaa, vcaa_taua
            real(F64) :: vcbb, vcbb_rhob, vcbb_sigbb, vcbb_taub

            vc = ZERO
            if (rho_a > MODRZEJ2016_RHO_THRESH .and. rho_b > MODRZEJ2016_RHO_THRESH) then
                  rho = rho_a + rho_b
                  !
                  ! d/dlambda Vc(lambda)|lambda->0
                  !
                  call vcab_0_deriv(vcab, vcab_rhoa, vcab_rhob, vcab_sigaa, vcab_sigab, vcab_sigbb, &
                        rho_a, rho_b, sigma_aa, sigma_ab, sigma_bb, MCSv2_Gopp, MCSv2_Fab)
                  call vcaa_0_deriv(vcaa, vcaa_rhoa, vcaa_sigaa, vcaa_taua, rho_a, sigma_aa, tau_a, MCSv2_Gpar, MCSv2_Faa)
                  call vcaa_0_deriv(vcbb, vcbb_rhob, vcbb_sigbb, vcbb_taub, rho_b, sigma_bb, tau_b, MCSv2_Gpar, MCSv2_Faa)
                  vc0 = vcaa + TWO * vcab + vcbb
                  epsc0 = vc0 / rho
                  !
                  ! Vc(lambda->Infinity)
                  !
                  call u_point_charge_c(epscinf, vcinf_rhoa, vcinf_rhob, rho_a, rho_b)
                  !
                  ! AC-D model (Eq. 55 in Ref. 2)
                  !
                  epsc = epscinf * epsc0 * lambda * (FOUR * epscinf + epsc0 * lambda) / (TWO * epscinf + epsc0 * lambda)**2
                  vc = rho * epsc
            else if (rho_a > MODRZEJ2016_RHO_THRESH) then
                  rho = rho_a
                  call vcaa_0_deriv(vcaa, vcaa_rhoa, vcaa_sigaa, vcaa_taua, rho_a, sigma_aa, tau_a, MCSv2_Gpar, MCSv2_Faa)
                  vc0 = vcaa
                  epsc0 = vc0 / rho
                  call u_point_charge_c(epscinf, vcinf_rhoa, vcinf_rhob, rho_a, ZERO)
                  epsc = epscinf * epsc0 * lambda * (FOUR * epscinf + epsc0 * lambda) / (TWO * epscinf + epsc0 * lambda)**2
                  vc = rho * epsc
            else if (rho_b > MODRZEJ2016_RHO_THRESH) then
                  rho = rho_b
                  call vcaa_0_deriv(vcbb, vcbb_rhob, vcbb_sigbb, vcbb_taub, rho_b, sigma_bb, tau_b, MCSv2_Gpar, MCSv2_Faa)
                  vc0 = vcbb
                  epsc0 = vc0 / rho
                  call u_point_charge_c(epscinf, vcinf_rhoa, vcinf_rhob, ZERO, rho_b)
                  epsc = epscinf * epsc0 * lambda * (FOUR * epscinf + epsc0 * lambda) / (TWO * epscinf + epsc0 * lambda)**2
                  vc = rho * epsc
            end if
      end subroutine modrzej2016_vc_lambda


      subroutine modrzej2012_vc_lambda(vc_aa, vc_ab, vc_bb, rho_a, rho_b, sigma_aa, sigma_ab, &
            sigma_bb, tau_a, tau_b, gpar, gopp, lambda)
            ! -------------------------------------------------------------------------
            ! Compute the adiabatic connection integrand Vc at a given value of lambda.
            ! The damping parameters Gopp and Gpar for the opposite- and parallel-spin
            ! components, respectively, can be nonequal (as opposed to the model
            ! of Ref. 1).
            ! -------------------------------------------------------------------------
            ! 1. Modrzejewski et al. J. Chem. Phys. 137, 204121 (2012);
            !    doi: 10.1063/1.4768228
            !
            real(F64), intent(out) :: vc_aa
            real(F64), intent(out) :: vc_ab
            real(F64), intent(out) :: vc_bb
            real(F64), intent(in)  :: rho_a
            real(F64), intent(in)  :: rho_b
            real(F64), intent(in)  :: sigma_aa
            real(F64), intent(in)  :: sigma_ab
            real(F64), intent(in)  :: sigma_bb
            real(F64), intent(in)  :: tau_a
            real(F64), intent(in)  :: tau_b
            real(F64), intent(in)  :: gpar
            real(F64), intent(in)  :: gopp
            real(F64), intent(in)  :: lambda

            real(F64) :: a_aa, b_aa, c_aa, d_aa
            real(F64) :: a_bb, b_bb, c_bb, d_bb
            real(F64) :: a_ab, b_ab, c_ab, d_ab
            
            if (rho_a > MODRZEJ2016_RHO_THRESH) then
                  call hcaa_eq16(a_aa, b_aa, c_aa, d_aa, rho_a, rho_b, sigma_aa, &
                        sigma_ab, sigma_bb, tau_a, gpar, lambda)
                  !
                  ! Eq. 41 in [1]
                  !
                  vc_aa = rho_a * PI * (EIGHT * b_aa + FOUR * a_aa * d_aa) / d_aa**5
            else
                  vc_aa = ZERO
            end if

            if (rho_a > MODRZEJ2016_RHO_THRESH .and. rho_b > MODRZEJ2016_RHO_THRESH) then
                  call hcab_eq16(a_ab, b_ab, c_ab, d_ab, rho_a, rho_b, sigma_aa, &
                        sigma_ab, sigma_bb, gopp, lambda)
                  !
                  ! Eq. 40 in [1]
                  !
                  vc_ab = rho_a * PI * (b_ab + a_ab * d_ab) / d_ab**3
            else
                  vc_ab = ZERO
            end if

            if (rho_b > MODRZEJ2016_RHO_THRESH) then
                  call hcaa_eq16(a_bb, b_bb, c_bb, d_bb, rho_b, rho_a, sigma_bb, &
                        sigma_ab, sigma_aa, tau_b, gpar, lambda)
                  !
                  ! Eq. 41 in [1]
                  !
                  vc_bb = rho_b * PI * (EIGHT * b_bb + FOUR * a_bb * d_bb) / d_bb**5
            else
                  vc_bb = ZERO
            end if
      end subroutine modrzej2012_vc_lambda


      pure subroutine modrzej2012_c_hole_average(t, npoints, spacing, rho, sigma, tau, grid_weight, gpar, gopp, lambda)
            ! ---------------------------------------------------------------------------------------------------
            ! Compute the system-averaged, spin-resolved correlation hole of Modrzejewski et al. At each point
            ! of space, the spin components of the correlation hole are defined as (Eq. 16 and Eq. 17)
            !
            ! h_ab(s) = (a_ab + b_ab * s + c_ab * s**2) * exp(-d_ab * s)
            ! h_aa(s) = s**2 (a_aa + b_aa * s + c_aa * s**2) * exp(-d_aa * s)
            !  
            ! The system-averaged holes are defined as 
            ! 
            ! <h_ab>(s) = <a_ab*exp(-d_ab * s)> + <b_ab*exp(-d_ab * s)> * s + <c_ab*exp(-d_ab * s)> * s**2
            ! <h_aa>(s) = s**2 (<a_aa*exp(-d_aa * s)> + <b_aa*exp(-d_aa * s)> * s + <c_aa*exp(-d_aa * s)> * s**2)
            !
            ! where <> denotes average value with the spin density rho_a (in the case of <h_bb> and <h_ba> it
            ! is the average with rho_b). A similar definition of the system-averaged hole is used by
            ! Constantin et al. (Eq. 2 in [2])) except the factor 1/N_sigma in front of the integral
            ! which is not present in our definition. The correlation energy at the given lambda may be 
            ! computed using <h_ab>, <h_aa>, and <h_bb> as follows: 
            !
            ! Ec = 1/2 * int_0^{inf} 4Pi s**2 ds (2 * <h_ab>(s) + <h_aa>(s) + <h_bb>(s)) / s
            !
            ! (see Eq. 3 in [2] and Eq. 4 in [3]). Note that <h_ab>(s) = <h_ba>(s). To get the full Kohn-Sham
            ! correlation energy, <h_ww'> must be integrated over the coupling constant parameter lambda.
            ! ----------------------------------------------------------------------------------------------------
            ! 1. Modrzejewski et al. J. Chem. Phys. 137, 204121 (2012); doi: 10.1063/1.4768228
            ! 2. Constantin et al. Phys. Rev. B 73, 205104 (2006); doi: 10.1103/PhysRevB.73.205104
            ! 3. Constantin et al. Phys. Rev. B 88, 125112 (2013); doi: 10.1103/PhysRevB.88.125112
            !
            real(F64), dimension(:), intent(inout) :: t
            integer, intent(in)                    :: npoints
            real(F64), intent(in)                  :: spacing
            real(F64), intent(in)                  :: rho
            real(F64), intent(in)                  :: sigma
            real(F64), intent(in)                  :: tau
            real(F64), intent(in)                  :: grid_weight
            real(F64), intent(in)                  :: gpar
            real(F64), intent(in)                  :: gopp
            real(F64), intent(in)                  :: lambda

            real(F64) :: rho_a, sigma_aa, tau_a
            real(F64) :: a_aa, b_aa, c_aa, d_aa
            real(F64) :: a_ab, b_ab, c_ab, d_ab
            real(F64) :: uk, hc_aa, hc_ab
            integer :: k
            real(F64), parameter :: rho_thresh = 2.0E-10_F64
            
            if (rho > rho_thresh) then
                  rho_a = rho/TWO
                  sigma_aa = sigma/FOUR
                  tau_a = tau/TWO
                  call hcab_eq16(a_ab, b_ab, c_ab, d_ab, rho_a, rho_a, sigma_aa, &
                        sigma_aa, sigma_aa, gopp, lambda)
                  call hcaa_eq16(a_aa, b_aa, c_aa, d_aa, rho_a, rho_a, sigma_aa, &
                        sigma_aa, sigma_aa, tau_a, gpar, lambda)

                  do k = 1, npoints
                        uk = (k-1) * spacing
                        hc_aa = uk**2 * (a_aa + b_aa * uk + c_aa * uk**2) * exp(-d_aa * uk)
                        t(k) = t(k) + grid_weight * rho_a * hc_aa
                  end do
                  
                  do k = 1, npoints
                        uk = (k-1) * spacing
                        hc_ab = (a_ab + b_ab * uk + c_ab * uk**2) * exp(-d_ab * uk)
                        t(npoints+k) = t(npoints+k) + grid_weight * rho_a * hc_ab
                  end do
            end if
      end subroutine modrzej2012_c_hole_average

      
      pure subroutine umodrzej2012_c_hole_average(t, npoints, spacing, rho_a, rho_b, sigma_aa, sigma_ab, &
            sigma_bb, tau_a, tau_b, grid_weight, gpar, gopp, lambda)
            !
            ! Compute the system-averaged, spin-resolved correlation hole of Modrzejewski et al.
            ! This is a spin-uncompensated variant of the subroutine. See comments for the spin-
            ! compensated variant. 
            !
            real(F64), dimension(:), intent(inout) :: t
            integer, intent(in)                    :: npoints
            real(F64), intent(in)                  :: spacing
            real(F64), intent(in)                  :: rho_a
            real(F64), intent(in)                  :: rho_b
            real(F64), intent(in)                  :: sigma_aa
            real(F64), intent(in)                  :: sigma_ab
            real(F64), intent(in)                  :: sigma_bb
            real(F64), intent(in)                  :: tau_a
            real(F64), intent(in)                  :: tau_b
            real(F64), intent(in)                  :: grid_weight
            real(F64), intent(in)                  :: gpar
            real(F64), intent(in)                  :: gopp
            real(F64), intent(in)                  :: lambda

            real(F64) :: a_aa, b_aa, c_aa, d_aa
            real(F64) :: a_bb, b_bb, c_bb, d_bb
            real(F64) :: a_ab, b_ab, c_ab, d_ab
            real(F64) :: uk, hc_ab, hc_aa, hc_bb
            integer :: k
            real(F64), parameter :: rho_thresh = 1.0E-10_F64
            
            if (rho_a > rho_thresh) then
                  call hcaa_eq16(a_aa, b_aa, c_aa, d_aa, rho_a, rho_b, sigma_aa, &
                        sigma_ab, sigma_bb, tau_a, gpar, lambda)
                  do k = 1, npoints
                        uk = (k-1) * spacing
                        hc_aa = uk**2 * (a_aa + b_aa * uk + c_aa * uk**2) * exp(-d_aa * uk)
                        t(k) = t(k) + grid_weight * rho_a * hc_aa
                  end do
            end if

            if (rho_a > rho_thresh .and. rho_b > rho_thresh) then
                  call hcab_eq16(a_ab, b_ab, c_ab, d_ab, rho_a, rho_b, sigma_aa, &
                        sigma_ab, sigma_bb, gopp, lambda)
                  do k = 1, npoints
                        uk = (k-1) * spacing
                        hc_ab = (a_ab + b_ab * uk + c_ab * uk**2) * exp(-d_ab * uk)
                        t(npoints+k) = t(npoints+k) + grid_weight * rho_a * hc_ab
                  end do
            end if

            if (rho_b > rho_thresh) then
                  call hcaa_eq16(a_bb, b_bb, c_bb, d_bb, rho_b, rho_a, sigma_bb, &
                        sigma_ab, sigma_aa, tau_b, gpar, lambda)
                  do k = 1, npoints
                        uk = (k-1) * spacing
                        hc_bb = uk**2 * (a_bb + b_bb * uk + c_bb * uk**2) * exp(-d_bb * uk)
                        t(2*npoints+k) = t(2*npoints+k) + grid_weight * rho_b * hc_bb
                  end do
            end if
      end subroutine umodrzej2012_c_hole_average


      pure subroutine bab_eq25(b_ab, rho_b, rs_ab, lambda)
            !
            ! Compute Bab(rhoa, rhob, lambda) defined in Eq. 25
            ! of J. Chem. Phys. 137, 204121 (2012); doi: 10.1063/1.4768228
            !
            real(F64), intent(out) :: b_ab
            real(F64), intent(in)  :: rho_b
            real(F64), intent(in)  :: rs_ab
            real(F64), intent(in)  :: lambda

            real(F64) :: t1, t2, t3, t4
            real(F64) :: r

            r = lambda * rs_ab
            t1 = 0.0207_F64 * r
            t2 = 0.08193_F64 * r**2
            t3 = -0.01277_F64 * r**3
            t4 = 0.001859_F64 * r**4
            b_ab = rho_b * (ONE + t1 + t2 + t3 + t4) * exp(-0.7524_F64 * r)
      end subroutine bab_eq25


      pure subroutine baa_eq32(b_aa, rs_aa, q_a, lambda)
            !
            ! Compute Baa(rhoa, sigma_aa, taua, lambda) defined in Eq. 31
            ! of J. Chem. Phys. 137, 204121 (2012); doi: 10.1063/1.4768228
            !
            real(F64), intent(out) :: b_aa
            real(F64), intent(in)  :: rs_aa
            real(F64), intent(in)  :: q_a
            real(F64), intent(in)  :: lambda

            real(F64) :: t1, t2, r

            r = lambda * rs_aa
            t1 = -0.01624_F64 * r
            t2 = 0.00264_F64 * r**2
            b_aa = q_a/THREE * (ONE + t1 + t2) * exp(-0.5566 * r)            
      end subroutine baa_eq32


      pure subroutine hcab_eq16(a_ab, b_ab, c_ab, d_ab, rho_a, rho_b, sigma_aa, &
            sigma_ab, sigma_bb, gparam, lambda)
            ! ------------------------------------------------------------------
            ! Compute the parameters a, b, c, and d of the opposite-spin,
            ! spherically-averaged correlation hole at an arbitrary value of 
            ! the coupling constant lambda (Eq. 16 in [1]). This subroutine can
            ! be supplied with an arbitrary positive value of the parameter G
            ! (Eq. 45)
            !
            ! No check of small values of the density is done. Make sure that
            ! rho_a > 1.0E-10 and rho_b > 1.0E-10 on entry to this subroutine.
            ! ------------------------------------------------------------------
            ! 1.  Modrzejewski et al., J. Chem. Phys. 137, 204121 (2012);
            !     doi: 10.1063/1.4768228
            !
            real(F64), intent(out) :: a_ab
            real(F64), intent(out) :: b_ab
            real(F64), intent(out) :: c_ab
            real(F64), intent(out) :: d_ab
            real(F64), intent(in)  :: rho_a
            real(F64), intent(in)  :: rho_b
            real(F64), intent(in)  :: sigma_aa
            real(F64), intent(in)  :: sigma_ab
            real(F64), intent(in)  :: sigma_bb
            real(F64), intent(in)  :: gparam
            real(F64), intent(in)  :: lambda

            real(F64), parameter :: seitz_coeff = (THREE/(FOUR*PI))**(ONE/THREE)
            real(F64), parameter :: rsab_coeff = (THREE/PI)**(ONE/THREE)
            real(F64), parameter :: fab = 2.1070d+0

            real(F64) :: rhotot, rhotot13, rhotot43, rhotot83, sigmatot
            real(F64) :: rho_a13, rho_b13
            real(F64) :: rs, rs_ab, w_ab

            rhotot = rho_a + rho_b
            rhotot13 = rhotot**(ONE/THREE)
            rhotot43 = rhotot13 * rhotot
            rhotot83 = rhotot43**2
            rs = seitz_coeff / rhotot13
            sigmatot = sigma_aa + sigma_bb + two * sigma_ab
            rho_a13 = rho_a**(ONE/THREE)
            rho_b13 = rho_b**(ONE/THREE)
            rs_ab = rsab_coeff / (rho_a13 + rho_b13)
            !
            ! Eq. 45
            !
            d_ab = fab / rs_ab + gparam *  sigmatot / (rs * rhotot83)
            !
            ! Eq. 32
            !
            call bab_eq25(w_ab, rho_b, rs_ab, lambda)
            a_ab = w_ab - rho_b
            !
            ! Eq. 33
            !
            b_ab = lambda * w_ab + d_ab * a_ab
            !
            ! Eq. 35
            !
            c_ab = -ONE/TWELVE * (a_ab * d_ab**2 + THREE * b_ab * d_ab)
      end subroutine hcab_eq16


      pure subroutine hcaa_eq16(a_aa, b_aa, c_aa, d_aa, rho_a, rho_b, sigma_aa, &
            sigma_ab, sigma_bb, tau_a, gparam, lambda)
            ! ------------------------------------------------------------------
            ! Compute the parameters a, b, c, and d of the parallel-spin,
            ! spherically-averaged correlation hole at an arbitrary value of 
            ! the coupling constant lambda (Eq.  in [1]). This subroutine can
            ! be supplied with an arbitrary positive value of the parameter G
            ! (Eq. 45).
            !
            ! No check of small values of the density is done. Make sure that
            ! rho_a > 1.0E-10 on entry to this subroutine.
            ! ------------------------------------------------------------------
            ! 1.  Modrzejewski et al., J. Chem. Phys. 137, 204121 (2012);
            !     doi: 10.1063/1.4768228
            !
            real(F64), intent(out) :: a_aa
            real(F64), intent(out) :: b_aa
            real(F64), intent(out) :: c_aa
            real(F64), intent(out) :: d_aa
            real(F64), intent(in)  :: rho_a
            real(F64), intent(in)  :: rho_b
            real(F64), intent(in)  :: sigma_aa
            real(F64), intent(in)  :: sigma_ab
            real(F64), intent(in)  :: sigma_bb
            real(F64), intent(in)  :: tau_a
            real(F64), intent(in)  :: gparam
            real(F64), intent(in)  :: lambda

            real(F64), parameter :: seitz_coeff = (THREE/(FOUR*PI))**(ONE/THREE)
            real(F64), parameter :: rsab_coeff = (THREE/PI)**(ONE/THREE)
            real(F64), parameter :: rsaa_coeff = ONE/TWO * rsab_coeff
            real(F64), parameter :: faa = 2.6422d+0

            real(F64) :: rhotot, rhotot13, rhotot43, rhotot83, sigmatot
            real(F64) :: rho_a13
            real(F64) :: rs, rs_aa, w_aa, q_a

            rhotot = rho_a + rho_b
            rhotot13 = rhotot**(ONE/THREE)
            rhotot43 = rhotot13 * rhotot
            rhotot83 = rhotot43**2
            rs = seitz_coeff / rhotot13
            sigmatot = sigma_aa + sigma_bb + two * sigma_ab
            rho_a13 = rho_a**(ONE/THREE)
            rs_aa = rsaa_coeff / rho_a13
            q_a = tau_a - frac14 * sigma_aa / rho_a
            !
            ! Eq. 45
            !
            d_aa = faa / rs_aa + gparam * sigmatot / (rs * rhotot83)
            !
            ! Eq. 36
            !
            call baa_eq32(w_aa, rs_aa, q_a, lambda)
            a_aa = w_aa - ONE/THREE * q_a
            !
            ! Eq. 37
            !
            b_aa = lambda / TWO * w_aa + a_aa * d_aa
            !
            ! Eq. 38
            !
            c_aa = -ONE/30.0_F64 * (a_aa * d_aa**2 + FIVE * b_aa * d_aa)
      end subroutine hcaa_eq16


      subroutine modrzej2012_vc_0_deriv(vc0, rho_a, rho_b, sigma_aa, sigma_ab, sigma_bb, tau_a, tau_b, gpar, gopp)
            !
            ! Compute the derivative dVc/dlambda|lambda->0 using the correlation functional
            ! of Modrzejewski et al. JCP 2012.
            !
            real(F64), intent(out) :: vc0
            real(F64), intent(in)  :: rho_a
            real(F64), intent(in)  :: rho_b
            real(F64), intent(in)  :: sigma_aa
            real(F64), intent(in)  :: sigma_ab
            real(F64), intent(in)  :: sigma_bb
            real(F64), intent(in)  :: tau_a
            real(F64), intent(in)  :: tau_b
            real(F64), intent(in)  :: gpar
            real(F64), intent(in)  :: gopp

            real(F64) :: vcab, vcab_rhoa, vcab_rhob, vcab_sigaa, vcab_sigab, vcab_sigbb
            real(F64) :: vcaa, vcaa_rhoa, vcaa_sigaa, vcaa_taua
            real(F64) :: vcbb, vcbb_rhob, vcbb_sigbb, vcbb_taub

            if (rho_a > MODRZEJ2016_RHO_THRESH .and. rho_b > MODRZEJ2016_RHO_THRESH) then                  
                  call vcab_0_deriv(vcab, vcab_rhoa, vcab_rhob, vcab_sigaa, vcab_sigab, vcab_sigbb, &
                        rho_a, rho_b, sigma_aa, sigma_ab, sigma_bb, gopp, MCSv2_Fab)
                  call vcaa_0_deriv(vcaa, vcaa_rhoa, vcaa_sigaa, vcaa_taua, rho_a, sigma_aa, tau_a, gpar, MCSv2_Faa)
                  call vcaa_0_deriv(vcbb, vcbb_rhob, vcbb_sigbb, vcbb_taub, rho_b, sigma_bb, tau_b, gpar, MCSv2_Faa)
                  vc0 = vcaa + TWO * vcab + vcbb
            else if (rho_a > MODRZEJ2016_RHO_THRESH) then
                  call vcaa_0_deriv(vcaa, vcaa_rhoa, vcaa_sigaa, vcaa_taua, rho_a, sigma_aa, tau_a, gpar, MCSv2_Faa)
                  vc0 = vcaa
            else if (rho_b > MODRZEJ2016_RHO_THRESH) then
                  call vcaa_0_deriv(vcbb, vcbb_rhob, vcbb_sigbb, vcbb_taub, rho_b, sigma_bb, tau_b, gpar, MCSv2_Faa)
                  vc0 = vcbb
            else
                  vc0 = ZERO
            end if
      end subroutine modrzej2012_vc_0_deriv
end module modrzej2016_xc_auxiliary
