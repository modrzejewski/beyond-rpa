module modrzej2016_x_hole
      use arithmetic
      use math_constants
      use modrzej2016_x_energy
      use pbe_x_energy
      use tpss_x_energy
      
      implicit none

contains

      pure subroutine br89_x_hole_average(t, npoints, spacing, rho, sigma, lapl, tau, grid_weight)
            !
            ! Compute the system-averaged, sigma-spin Becke-Roussel exchange hole
            ! at given distances from the reference point. The weight function of
            ! the system average is the sigma-spin density without the factor 1/N.
            !  
            real(F64), dimension(:), intent(inout) :: t
            integer, intent(in)                    :: npoints
            real(F64), intent(in)                  :: spacing
            real(F64), intent(in)                  :: rho
            real(F64), intent(in)                  :: sigma
            real(F64), intent(in)                  :: lapl
            real(F64), intent(in)                  :: tau
            real(F64), intent(in)                  :: grid_weight

            real(F64) :: a, b, rk, hx, rho_a, sigma_aa, tau_a, lapl_a
            integer :: k
            real(F64), parameter :: rho_thresh = 1.0E-10_F64
            
            if (rho > rho_thresh) then
                  rho_a = rho/TWO
                  sigma_aa = sigma/FOUR
                  lapl_a = lapl/TWO
                  tau_a = tau/TWO
                  call br89_get_ab(a, b, rho_a, sigma_aa, lapl_a, tau_a)
                  do k = 1, npoints
                        rk = (k-1) * spacing
                        call br89_x_hole_ab(hx, rk, a, b)
                        t(k) = t(k) + grid_weight * rho_a * hx
                  end do
            end if
      end subroutine br89_x_hole_average

      
      pure subroutine br89_x_hole_ab(hx, r, a, b)
            !
            ! Compute the sigma-spin Becke-Roussel exchange hole for a given
            ! pair of the parameters (a, b) and at a given radius r.
            ! The computed exchange hole hx integrates to -1. 
            !
            real(F64), intent(out) :: hx
            real(F64), intent(in)  :: r
            real(F64), intent(in)  :: a
            real(F64), intent(in)  :: b

            real(F64) :: expx, x, t1, t2, r2
            real(F64) :: w0, w2, w4, w6, w8
            
            if (r < b .and. r * a < 0.1_F64) then
                  x = a * b
                  expx = exp(-x)
                  w0 = -a**3*expx/(8.0_F64*PI)
                  w2 = -(a**4*(-2.0_F64+x)*expx)/(48.0_F64*b*PI)
                  w4 = -(a**6*(-4.0_F64+x)*expx)/(960.0_F64*b*PI)
                  w6 = -(a**8*(-6.0_F64+x)*expx)/(40320.0_F64*b*PI)
                  w8 = -(a**10*(-8.0_F64+x)*expx)/(2903040.0_F64*b*PI)
                  r2 = r**2
                  hx = w8
                  hx = w6 + r2 * hx
                  hx = w4 + r2 * hx
                  hx = w2 + r2 * hx
                  hx = w0 + r2 * hx
            else
                  t1 = (a * abs(b - r) + 1.0_F64) * exp(-a * abs(b - r))
                  t2 = (a * (b + r) + 1.0_F64) * exp(-a * (b + r))
                  hx = -a / (16.0_F64 * PI * b * r) * (t1 - t2)
            end if
      end subroutine br89_x_hole_ab


      pure subroutine lda_x_hole(hx, r, rhos)
            !
            ! Compute the exact sigma-spin LDA exchange hole for a given
            ! electron spin density rhos,
            ! hx(r) = -9 rhos * j1(kf * r)**2 / r**2,
            ! where j1 is a spherical Bessel function of the first kind,
            ! j1(x) = sin(x)/x**2 - cos(x)/x. hx(r) integrates to -1,
            ! 4Pi int r**2 hx(r) = -1.
            !
            real(F64), intent(out) :: hx
            real(F64), intent(in)  :: r
            real(F64), intent(in)  :: rhos

            real(F64) :: sinx, cosx, x, kf, x2
            real(F64), parameter :: c0 = 0.1111111111111111_F64
            real(F64), parameter :: c2 = -0.02222222222222222_F64
            real(F64), parameter :: c4 = 0.001904761904761905_F64
            real(F64), parameter :: c6 = -0.00009406231628453851_F64
            real(F64), parameter :: c8 = 3.053971307939562E-6_F64
            real(F64), parameter :: c10 = -7.047626095245143E-8_F64
            real(F64), parameter :: c12 = 1.218108213992988E-9_F64

            kf = (SIX * PI**2 * rhos)**(ONE/THREE)
            x = kf * r
            if (x > 0.4_F64) then
                  sinx = sin(x)
                  cosx = cos(x)
                  hx = -NINE * rhos * (sinx / x - cosx)**2 / x**4
            else
                  x2 = x**2
                  hx = c12
                  hx = c10 + x2 * hx
                  hx = c8 + x2 * hx
                  hx = c6 + x2 * hx
                  hx = c4 + x2 * hx
                  hx = c2 + x2 * hx
                  hx = c0 + x2 * hx
                  hx = -9.0_F64 * rhos * hx
            end if
      end subroutine lda_x_hole


      pure subroutine br89_get_ab(a, b, rho, sigma, lapl, tau)
            ! ------------------------------------------------------------------
            ! Compute the parameters (a, b) of the Becke-Roussel sigma-spin
            ! exchange hole. 
            ! ------------------------------------------------------------------
            ! 1. Becke, A.D., Roussel, M.R., Exchange holes in inhomogeneous
            !    systems: A coordinate-space model, Phys. Rev. A, 39,
            !    3761(1989); doi: 10.1103/PhysRevA.39.3761
            ! ------------------------------------------------------------------
            ! A, B
            !        Parameters of the Becke-Roussel exchange hole
            ! RHO    
            !        Electronic density (sigma spin)
            ! SIGMA  
            !        (Nabla Rho) * (Nabla Rho), length of the sigma-sigma spin
            !        density gradient
            ! LAPL   
            !        Nabla^2 Rho, Laplacian of the sigma-spin electron density
            ! TAU    
            !        Sigma-spin kinetic energy density without the 1/2 factor,
            !        defined in Eq. 13c in [1]
            !
            real(F64), intent(out) :: a
            real(F64), intent(out) :: b
            real(F64), intent(in)  :: rho
            real(F64), intent(in)  :: sigma
            real(F64), intent(in)  :: lapl
            real(F64), intent(in)  :: tau

            integer :: i
            real(F64) :: xpos, xneg, diff, rhs, y, x, d, q
            integer, parameter :: max_niter = 1000
            real(F64), parameter :: xthresh = 5.0E-15_F64
            !
            ! Eq. 13b in [1]
            !
            d = tau - ONE/FOUR * sigma / rho
            !
            ! Eq. 20b in [1]
            !
            q = ONE/SIX * (lapl - TWO * d)
            !
            ! Eq. 21 in [1]
            !
            rhs = TWO/THREE * PI**(TWO/THREE) * rho**(FIVE/THREE) / q
            !
            ! Use the bijection method to find x
            !
            if (q > ZERO) then
                  !
                  ! Positive branch of y(x)
                  !
                  xpos = TWO
                  xneg = FIVE
                  y = xneg * exp(-TWO/THREE * xneg) / (xneg - TWO)
                  do while (.not. y < rhs)
                        xneg = xneg * TWO
                        y = xneg * exp(-TWO/THREE * xneg) / (xneg - TWO)
                  end do
                  x = (xpos + xneg) / TWO
                  do i = 1, max_niter
                        y = x * exp(-TWO/THREE * x)
                        diff = y - rhs * (x - TWO)
                        if (diff > ZERO) then
                              xpos = x
                        else
                              xneg = x
                        end if
                        x = (xpos + xneg) / TWO
                        if (abs(xpos - xneg) < abs(xthresh * x)) then
                              exit
                        end if
                  end do
            else
                  !
                  ! Negative branch of y(x)
                  !
                  xpos = ZERO
                  xneg = TWO
                  x = (xpos + xneg) / TWO
                  do i = 1, max_niter
                        y = x * exp(-TWO/THREE * x)
                        diff = y - rhs * (x - TWO)
                        if (diff > ZERO) then
                              xneg = x
                        else
                              xpos = x
                        end if
                        x = (xpos + xneg) / TWO
                        if (abs(xpos - xneg) < abs(xthresh * x)) then
                              exit
                        end if
                  end do
            end if
            !
            ! Eq. 22 in [1]
            !
            b = x * exp(-x/THREE) / (TWO * PI**(ONE/THREE) * rho**(ONE/THREE))
            a = x / b
      end subroutine br89_get_ab


      subroutine br89_x_hole_average_general(t, npoints, spacing, rho, sigma, lapl, tau, grid_weight, energy_density)
            !
            ! Compute the system-averaged, sigma-spin generalized Becke-Roussel
            ! exchange hole at given distances from the reference point.
            ! The computed Becke-Roussel exchange hole integrates to
            ! the provided base exchange energy density, has the exact second-order
            ! expansion, but integrates to -N instead of -1. The weight function
            ! of the system average is the sigma-spin density without the factor 1/Nel.
            !  
            real(F64), dimension(:), intent(inout) :: t
            integer, intent(in)                    :: npoints
            real(F64), intent(in)                  :: spacing
            real(F64), intent(in)                  :: rho
            real(F64), intent(in)                  :: sigma
            real(F64), intent(in)                  :: lapl
            real(F64), intent(in)                  :: tau
            real(F64), intent(in)                  :: grid_weight
            real(F64), intent(in)                  :: energy_density

            real(F64) :: Q, rhs, a, b, x, x_rhs, a2, N, expx, hx, rk
            integer :: k
            logical :: converged
            real(F64), parameter :: convthresh = 1.0E-14_F64
            !
            ! Compute the second-order term of the expansion
            ! hx(r, s) = 1/2 rho - Q * s**2 + ...
            !
            Q = (1.0_F64/12.0_F64) * lapl - (1.0_F64/6.0_F64) * tau + (1.0_F64/24.0_F64) * sigma / rho
            rhs = -6.0_F64/PI * Q / rho**2 * energy_density
            !
            ! Find the parameter x of the Becke-Roussel hole (Eq. A7 in Ref. 1)
            !
            call x_solver_exact_curvature(x, x_rhs, rhs, converged, convthresh)
            !
            ! Compute a**2 by solving Eq. A4 for a**2. This method is more numerically
            ! stable than Eq. A6. (The numerical stability tested with Mathematica for
            ! Qs close to zero.)
            !
            expx = exp(x)
            a2 = PI * rho * (TWO - TWO * expx + x) / (x * energy_density)
            a = sqrt(a2)
            b = x / a
            !
            ! Normalization factor (Eq. A8 in Ref. 1)
            !
            N = FOUR*PI * rho * expx / a**3
            do k = 1, npoints
                  rk = (k-1) * spacing
                  call br89_x_hole_ab(hx, rk, a, b)
                  t(k) = t(k) + grid_weight * (rho/TWO) * N * hx
            end do
      end subroutine br89_x_hole_average_general

      
      subroutine pbe_x_hole_average(t, npoints, spacing, rho, sigma, lapl, tau, grid_weight)
            !
            ! Compute the system-averaged, sigma-spin generalized Becke-Roussel
            ! exchange hole at given distances from the reference point.
            ! The computed Becke-Roussel exchange hole integrates to
            ! the PBE exchange energy density, has the exact second-order
            ! expansion, but integrates to -N instead of -1. The weight function
            ! of the system average is the sigma-spin density without the factor 1/Nel.
            !  
            real(F64), dimension(:), intent(inout) :: t
            integer, intent(in)                    :: npoints
            real(F64), intent(in)                  :: spacing
            real(F64), intent(in)                  :: rho
            real(F64), intent(in)                  :: sigma
            real(F64), intent(in)                  :: lapl
            real(F64), intent(in)                  :: tau
            real(F64), intent(in)                  :: grid_weight
            
            real(F64) :: eps, e_rho, e_sigma

            if (rho > PBE_X_RHO_THRESH) then
                  call pbe_x(eps, e_rho, e_sigma, rho, sigma)
                  call br89_x_hole_average_general(t, npoints, spacing, rho, sigma, lapl, tau, grid_weight, eps)
            end if
      end subroutine pbe_x_hole_average


      subroutine tpss_x_hole_average(t, npoints, spacing, rho, sigma, lapl, tau, grid_weight)
            !
            ! Compute the system-averaged, sigma-spin generalized Becke-Roussel
            ! exchange hole at given distances from the reference point.
            ! The computed Becke-Roussel exchange hole integrates to
            ! the PBE exchange energy density, has the exact second-order
            ! expansion, but integrates to -N instead of -1. The weight function
            ! of the system average is the sigma-spin density without the factor 1/Nel.
            !  
            real(F64), dimension(:), intent(inout) :: t
            integer, intent(in)                    :: npoints
            real(F64), intent(in)                  :: spacing
            real(F64), intent(in)                  :: rho
            real(F64), intent(in)                  :: sigma
            real(F64), intent(in)                  :: lapl
            real(F64), intent(in)                  :: tau
            real(F64), intent(in)                  :: grid_weight
            
            real(F64) :: eps, e_rho, e_sigma, e_tau

            if (rho > TPSS_X_RHO_THRESH .and. tau > TPSS_X_TAU_THRESH) then
                  call tpss_x(eps, e_rho, e_sigma, e_tau, rho, sigma, tau)
                  call br89_x_hole_average_general(t, npoints, spacing, rho, sigma, lapl, tau, grid_weight, eps)
            end if
      end subroutine tpss_x_hole_average
end module modrzej2016_x_hole
