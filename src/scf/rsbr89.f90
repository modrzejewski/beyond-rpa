module rsbr89
      use arithmetic
      use math_constants

      implicit none
      save
      !
      ! Hydrogenic atom, gamma = 1.0 (consistency with Q-Chem, Gaussian 09)
      ! Uniform electron gas, gamma = 0.8
      ! (see the Becke-Roussel paper for details)
      !
      real(F64), parameter, private :: gamma = 1.0d+0
      !
      ! Default values of range separation parameter (OMEGA) and
      ! the default portion of exact exchange in the short range
      !
      real(F64), parameter :: BR89_OMEGA0 = 0.45d+0
      real(F64), parameter :: BR89_SREXX0 = 0.30d+0

      real(F64) :: BR89_OMEGA = BR89_OMEGA0
      real(F64) :: BR89_SREXX = BR89_SREXX0
      !
      ! If sigma density at a given point falls below RHOTOL, 
      ! the evaluation of the functional and its derivatives
      ! is skipped
      !
      real(F64), parameter, private :: RHOTOL = 1.d-10
      real(F64), parameter, private :: TAUTOL = 1.d-10

contains


      subroutine rsbr89_srexx(c)
            !
            ! Return default portion of short-range exact exchange in 
            ! range separated BR89 exchange functional.
            !
            real(F64), intent(out) :: c
            c = BR89_SREXX0
      end subroutine rsbr89_srexx
      

      subroutine rsbr89_setsrexx(c)
            !
            ! Set user-defined value of short-range exact exchange
            !
            real(F64), intent(in) :: c
            BR89_SREXX = c
      end subroutine rsbr89_setsrexx


      subroutine rsbr89_omega(c)
            !
            ! Return deafult value of range separation parameter (OMEGA).
            !
            real(F64), intent(out) :: c
            c = BR89_OMEGA0
      end subroutine rsbr89_omega


      subroutine rsbr89_setomega(c)
            ! 
            ! Set user-defined value of range separation parameter (OMEGA)
            !
            real(F64), intent(in) :: c
            BR89_OMEGA = c
      end subroutine rsbr89_setomega
      

      subroutine srbr89_inout(rho, sigma, lapl, tau, eps, vrho, vsigma, vlapl, vtau, npt)
            ! ---------------------------------------------------------------------
            ! Short-range Becke-Roussel 89 exchange functional.
            ! ---------------------------------------------------------------------
            ! 1. Becke, A.D., Roussel, M.R., Exchange holes in inhomogeneous
            !    systems: A coordinate-space model, Phys. Rev. A, 39,
            !    3761(1989).
            ! 2. Toyoda, M. and Ozaki, T., Exchange functional by a range-
            !    separated exchange hole, Phys. Rev. A 83, 032515 (2011)
            !    [We corrected Eq. 14 in Ref. 2]
            !
            real(F64), dimension(:), intent(in)    :: rho
            real(F64), dimension(:), intent(in)    :: sigma
            real(F64), dimension(:), intent(in)    :: lapl
            real(F64), dimension(:), intent(in)    :: tau
            real(F64), dimension(:), intent(inout) :: eps
            real(F64), dimension(:), intent(inout) :: vrho
            real(F64), dimension(:), intent(inout) :: vsigma
            real(F64), dimension(:), intent(inout) :: vlapl
            real(F64), dimension(:), intent(inout) :: vtau
            integer, intent(in)                    :: npt

            integer :: k
            real(F64) :: rhoa, sigmaa, lapla, taua
            real(F64) :: eps0, vrho0, vsigma0, vlapl0, vtau0
            real(F64) :: slx

            slx = one - BR89_SREXX

            do k = 1, npt
                  rhoa = frac12 * rho(k)
                  sigmaa = frac14 * sigma(k)
                  lapla = frac12 * lapl(k)
                  taua = frac12 * tau(k)

                  if (rhoa > RHOTOL .and. taua > TAUTOL) then
                        call usrxbr89(rhoa, sigmaa, lapla, taua, BR89_OMEGA, &
                              eps0, vrho0, vsigma0, vlapl0, vtau0)
                        eps(k) = eps(k) + slx * eps0
                        vrho(k) = vrho(k) + slx * vrho0
                        !
                        ! d sigmaa / d sigma = 1/4
                        !
                        vsigma(k) = vsigma(k) + slx * frac12 * vsigma0
                        vlapl(k) = vlapl(k) + slx * vlapl0
                        vtau(k) = vtau(k) + slx * vtau0
                  end if
            end do
      end subroutine srbr89_inout


      subroutine srbr89_out(rho, sigma, lapl, tau, eps, vrho, vsigma, vlapl, vtau, npt)
            ! ---------------------------------------------------------------------
            ! Short-range Becke-Roussel 89 exchange functional.
            ! ---------------------------------------------------------------------
            ! 1. Becke, A.D., Roussel, M.R., Exchange holes in inhomogeneous
            !    systems: A coordinate-space model, Phys. Rev. A, 39,
            !    3761(1989).
            ! 2. Toyoda, M. and Ozaki, T., Exchange functional by a range-
            !    separated exchange hole, Phys. Rev. A 83, 032515 (2011)
            !    [We corrected Eq. 14 in Ref. 2]
            !
            real(F64), dimension(:), intent(in)  :: rho
            real(F64), dimension(:), intent(in)  :: sigma
            real(F64), dimension(:), intent(in)  :: lapl
            real(F64), dimension(:), intent(in)  :: tau
            real(F64), dimension(:), intent(out) :: eps
            real(F64), dimension(:), intent(out) :: vrho
            real(F64), dimension(:), intent(out) :: vsigma
            real(F64), dimension(:), intent(out) :: vlapl
            real(F64), dimension(:), intent(out) :: vtau
            integer, intent(in)                         :: npt

            integer :: k
            real(F64) :: rhoa, sigmaa, lapla, taua
            real(F64) :: eps0, vrho0, vsigma0, vlapl0, vtau0
            real(F64) :: slx

            slx = one - BR89_SREXX

            do k = 1, npt
                  rhoa = frac12 * rho(k)
                  sigmaa = frac14 * sigma(k)
                  lapla = frac12 * lapl(k)
                  taua = frac12 * tau(k)

                  if (rhoa > RHOTOL .and. taua > TAUTOL) then
                        call usrxbr89(rhoa, sigmaa, lapla, taua, BR89_OMEGA, &
                              eps0, vrho0, vsigma0, vlapl0, vtau0)
                        eps(k) = slx * eps0
                        vrho(k) = slx * vrho0
                        !
                        ! d sigmaa / d sigma = 1/4
                        !
                        vsigma(k) = slx * frac12 * vsigma0
                        vlapl(k) = slx * vlapl0
                        vtau(k) = slx * vtau0
                  else
                        eps(k) = ZERO
                        vrho(k) = ZERO
                        vsigma(k) = ZERO
                        vlapl(k) = ZERO
                        vtau(k) = ZERO
                  end if
            end do
      end subroutine srbr89_out


      subroutine usrbr89_inout(rho, sigma, lapl, tau, eps, vrho, vsigma, &
            vlapl, vtau, npt)
            
            real(F64), dimension(:, :), intent(in)    :: rho
            real(F64), dimension(:, :), intent(in)    :: sigma
            real(F64), dimension(:, :), intent(in)    :: lapl
            real(F64), dimension(:, :), intent(in)    :: tau
            real(F64), dimension(:), intent(inout)    :: eps
            real(F64), dimension(:, :), intent(inout) :: vrho
            real(F64), dimension(:, :), intent(inout) :: vsigma
            real(F64), dimension(:, :), intent(inout) :: vlapl
            real(F64), dimension(:, :), intent(inout) :: vtau
            integer, intent(in)                       :: npt

            integer :: k
            real(F64) :: epsa, vrhoa, vsigma_aa, vlapla, vtaua
            real(F64) :: epsb, vrhob, vsigma_bb, vlaplb, vtaub
            real(F64) :: wa, wb, slx
            real(F64) :: rhotot

            slx = one - BR89_SREXX

            do k = 1, npt
                  rhotot = rho(1, k) + rho(2, k)
                  
                  if (rho(1, k) > RHOTOL .and. tau(1, k) > TAUTOL) then
                        call usrxbr89(rho(1, k), sigma(1, k), lapl(1, k), tau(1, k), &
                              BR89_OMEGA, epsa, vrhoa, vsigma_aa, vlapla, vtaua)
                        vrho(1, k) = vrho(1, k) + slx * vrhoa
                        vsigma(1, k) = vsigma(1, k) + slx * vsigma_aa
                        vlapl(1, k) = vlapl(1, k) + slx * vlapla
                        vtau(1, k) = vtau(1, k) + slx * vtaua
                        wa = rho(1, k) / rhotot
                  else
                        wa = zero
                        epsa = zero
                  end if

                  if (rho(2, k) > RHOTOL .and. tau(2, k) > TAUTOL) then
                        call usrxbr89(rho(2, k), sigma(3, k), lapl(2, k), tau(2, k), &
                              BR89_OMEGA, epsb, vrhob, vsigma_bb, vlaplb, vtaub)
                        vrho(2, k) = vrho(2, k) + slx * vrhob
                        vsigma(3, k) = vsigma(3, k) + slx * vsigma_bb
                        vlapl(2, k) = vlapl(2, k) + slx * vlaplb
                        vtau(2, k) = vtau(2, k) + slx * vtaub
                        wb = rho(2, k) / rhotot
                  else
                        wb = zero
                        epsb = zero
                  end if

                  eps(k) = eps(k) + slx * (wa * epsa + wb * epsb)
            end do
      end subroutine usrbr89_inout


      subroutine usrbr89_out(rho, sigma, lapl, tau, eps, vrho, vsigma, &
            vlapl, vtau, npt)
            
            real(F64), dimension(:, :), intent(in)  :: rho
            real(F64), dimension(:, :), intent(in)  :: sigma
            real(F64), dimension(:, :), intent(in)  :: lapl
            real(F64), dimension(:, :), intent(in)  :: tau
            real(F64), dimension(:), intent(out)    :: eps
            real(F64), dimension(:, :), intent(out) :: vrho
            real(F64), dimension(:, :), intent(out) :: vsigma
            real(F64), dimension(:, :), intent(out) :: vlapl
            real(F64), dimension(:, :), intent(out) :: vtau
            integer, intent(in)                            :: npt

            integer :: k
            real(F64) :: epsa, vrhoa, vsigma_aa, vlapla, vtaua
            real(F64) :: epsb, vrhob, vsigma_bb, vlaplb, vtaub
            real(F64) :: wa, wb, slx
            real(F64) :: rhotot

            slx = one - BR89_SREXX

            do k = 1, npt
                  !
                  ! Mixed derivatives equal ZERO
                  !
                  vsigma(2, k) = ZERO
                  rhotot = rho(1, k) + rho(2, k)
                  
                  if (rho(1, k) > RHOTOL .and. tau(1, k) > TAUTOL) then
                        call usrxbr89(rho(1, k), sigma(1, k), lapl(1, k), tau(1, k), &
                              BR89_OMEGA, epsa, vrhoa, vsigma_aa, vlapla, vtaua)
                        vrho(1, k) = slx * vrhoa
                        vsigma(1, k) = slx * vsigma_aa
                        vlapl(1, k) = slx * vlapla
                        vtau(1, k) = slx * vtaua
                        wa = rho(1, k) / rhotot
                  else
                        vrho(1, k) = ZERO
                        vsigma(1, k) = ZERO
                        vlapl(1, k) = ZERO
                        vtau(1, k) = ZERO
                        wa = zero
                        epsa = zero
                  end if

                  if (rho(2, k) > RHOTOL .and. tau(2, k) > TAUTOL) then
                        call usrxbr89(rho(2, k), sigma(3, k), lapl(2, k), tau(2, k), &
                              BR89_OMEGA, epsb, vrhob, vsigma_bb, vlaplb, vtaub)
                        vrho(2, k) = slx * vrhob
                        vsigma(3, k) = slx * vsigma_bb
                        vlapl(2, k) = slx * vlaplb
                        vtau(2, k) = slx * vtaub
                        wb = rho(2, k) / rhotot
                  else
                        vrho(2, k) = ZERO
                        vsigma(3, k) = ZERO
                        vlapl(2, k) = ZERO
                        vtau(2, k) = ZERO
                        wb = zero
                        epsb = zero
                  end if

                  eps(k) = slx * (wa * epsa + wb * epsb)
            end do
      end subroutine usrbr89_out


      subroutine usrxbr89(rho, sigma, lapl, tau, omega, eps, vrho, vsigma, &
            vlapl, vtau)
            ! --------------------------------------------------------------
            ! Becke-Roussel 1989 exchange functional. Solution of nonlinear
            ! equation using numerical fit of Proynov, Gan, Kong. This
            ! subroutine can be used for both spin-polarized and
            ! spin-unpolarized case.
            ! --------------------------------------------------------------
            ! 1. Becke, A.D., Roussel, M.R., Exchange holes in inhomogeneous
            !    systems: A coordinate-space model, Phys. Rev. A, 39,
            !    3761(1989).
            ! 2. Proynov, E., Gan, Z., Kong, J., Analytical representation
            !    of the Becke-Roussel exchange functional, Chem. Phys. Lett.
            !    455, 103(2008).
            ! --------------------------------------------------------------
            ! All of the above params should refer to single-spin density.
            !
            ! RHO    - Electronic density
            ! SIGMA  - GRAD(1)**2 + GRAD(2)**2 + GRAD(3)**2
            ! LAPL   - Laplasian of density
            ! TAU    - Two times kinetic energy density, see def. 13c in [1]
            ! OMEGA  - Range separation parameter
            ! EPS    - Density of exchange energy
            ! VRHO   - RHO derivative of exchange energy
            ! VSIGMA - SIGMA derivative of exchange energy
            ! VLAPL  - LAPL derivative of exchange energy
            ! VTAU   - TAU derivative of exchange energy
            !
            real(F64), intent(in)  :: rho
            real(F64), intent(in)  :: sigma
            real(F64), intent(in)  :: lapl
            real(F64), intent(in)  :: tau
            real(F64), intent(in)  :: omega
            real(F64), intent(out) :: eps
            real(F64), intent(out) :: vrho
            real(F64), intent(out) :: vsigma
            real(F64), intent(out) :: vlapl
            real(F64), intent(out) :: vtau

            real(F64) :: x, d, q
            real(F64) :: exm, exm13, exm23
            real(F64) :: aa, bb, cc, dd
            real(F64) :: yx
            real(F64) :: rho13, rho23, rho53, halfrho
            real(F64) :: qrho, qsigma
            real(F64) :: xrho, xsigma, xlapl, xtau
            real(F64) :: y, y2, y3, y4, y5
            real(F64) :: p1, p2, g, by
            real(F64) :: xnew, yhat
            real(F64) :: uxtot, uxtot_a, uxtot_b
            real(F64) :: uxlr, uxlr_a, uxlr_b
            real(F64) :: uxsr, uxsr_a, uxsr_b
            real(F64) :: uxsr_x, uxsr_rho
            real(F64) :: uxsr_sigma, uxsr_tau, uxsr_lapl
            real(F64) :: brhole_a, brhole_b
            real(F64) :: brhole_a_x, brhole_a_rho
            real(F64) :: brhole_b_x, brhole_b_rho
            integer          :: i
            !
            ! Partial derivatives of Q variable
            !
            real(F64), parameter :: qlapl = frac16
            real(F64), parameter :: qtau = -frac13 * gamma
            !
            ! Parameters of numerical fit
            !
            real(F64), parameter :: b  = 2.085749716493756d+0

            real(F64), parameter :: a1 = 1.5255251812009530d+0
            real(F64), parameter :: a2 = 4.576575543602858d-1
            real(F64), parameter :: a3 = 4.292036732051034d-1

            real(F64), parameter :: b0 = 4.771976183772063d-1
            real(F64), parameter :: b1 = -1.7799813494556270d+0
            real(F64), parameter :: b2 = 3.8433841862302150d+0
            real(F64), parameter :: b3 = -9.5912050880518490d+0
            real(F64), parameter :: b4 = 2.1730180285916720d+0
            real(F64), parameter :: b5 = -30.425133851603660d+0

            real(F64), parameter :: c0 = 7.566445420735584d-1
            real(F64), parameter :: c1 = -2.6363977871370960d+0
            real(F64), parameter :: c2 = 5.4745159964232880d+0
            real(F64), parameter :: c3 = -12.657308127108290d+0
            real(F64), parameter :: c4 = 4.1250584725121360d+0
            real(F64), parameter :: c5 = -30.425133957163840d+0

            real(F64), parameter :: d0 = 4.435009886795587d-5
            real(F64), parameter :: d1 = 5.8128653604457910d-1
            real(F64), parameter :: d2 = 66.742764515940610d+0
            real(F64), parameter :: d3 = 434.26780897229770d+0
            real(F64), parameter :: d4 = 824.77657660522390d+0
            real(F64), parameter :: d5 = 1657.9652731582120d+0

            real(F64), parameter :: e0 = 3.347285060926091d-5
            real(F64), parameter :: e1 = 4.7917931023971350d-1
            real(F64), parameter :: e2 = 62.392268338574240d+0
            real(F64), parameter :: e3 = 463.14816427938120d+0
            real(F64), parameter :: e4 = 785.23603501040290d+0
            real(F64), parameter :: e5 = 1657.9629682232730d+0
            !
            ! Convergence control of Newton iterations
            !
            integer, parameter :: niter = 100
            real(F64), parameter :: thresh = 1.d-14
            real(F64), parameter :: xthresh = 1.d-14
            real(F64), parameter :: thresh_singular = 0.01d+0
            real(F64) :: xpositive, xnegative, diff
            !
            ! Eq. 13b in [1]
            !
            d = tau - frac14 * sigma / rho
            !
            ! d variable should always be nonnegative
            ! due to Schwarz inequality. This constraint may
            ! be violated if kinetic energy density, gradient,
            ! and density are computed approximately.
            !
            d = max(zero, d)
            !
            ! Eq. 20b in [1]
            !
            q = frac16 * (lapl - 2.d+0 * gamma * d)
            rho13 = rho**(1.d+0 / 3.d+0)
            rho23 = rho13**2
            rho53 = rho23 * rho23 * rho13
            !
            ! Calculate x using analytic expression
            !
            y = frac23 * pi23 * rho53 / q
            y2 = y * y
            y3 = y2 * y
            y4 = y2 * y2
            y5 = y4 * y

            if (y .le. zero) then
                  p1 = c0 + c1 * y + c2 * y2 + c3 * y3 + c4 * y4 + c5 * y5
                  p2 = b0 + b1 * y + b2 * y2 + b3 * y3 + b4 * y4 + b5 * y5
                  g = -atan(a1 * y + a2) + a3
            else
                  p1 = d0 + d1 * y + d2 * y2 + d3 * y3 + d4 * y4 + d5 * y5
                  p2 = e0 + e1 * y + e2 * y2 + e3 * y3 + e4 * y4 + e5 * y5
                  by = b * y
                  !
                  ! Arccsch(x) = ln((1 + sqrt(1 + x**2)) / x) , x > 0
                  !
                  g = log((one + sqrt(one + by**2)) / by) + two
            end if
            !
            ! x value calculated by numerical analytic fit
            !
            x = g * p1 / p2
            !
            ! Refine the approximate X evaluated
            ! above
            !
            if (abs(x - two) .gt. thresh_singular) then
                  !
                  ! Faster, but numerically less stable
                  ! method
                  !
                  do i = 1, niter
                        exm23 = exp(-frac23 * x)
                        yhat = x * exm23 / (x - two)
                        !
                        ! Check convergence
                        !
                        diff = abs((yhat - y) / y)
                        if (diff .lt. thresh) exit

                        yx = one + (one - two * x) / (x**2 - two * x + three)
                        yx = -frac32 * yx / exm23
                        xnew = x - (yhat - y) * yx
                        x = xnew
                  end do
            else
                  !
                  ! Robust bijection method
                  !
                  if (x .gt. two) then
                        !
                        ! Positive branch of y(x)
                        !
                        xpositive = two
                        xnegative = two * (one + thresh_singular)

                        do i = 1, niter
                              x = (xpositive + xnegative) / two
                              yhat = x * exp(-frac23 * x)
                              diff = yhat - y * (x - two)

                              if (diff .gt. zero) then
                                    xpositive = x
                              else
                                    xnegative = x
                              end if

                              if (abs((xpositive - xnegative) / xpositive) .lt. xthresh) then
                                    exit
                              end if
                        end do
                  else
                        !
                        ! Negative branch of y(x)
                        !
                        xpositive = -two * (one + thresh_singular)
                        xnegative = two

                        do i = 1, niter
                              x = (xpositive + xnegative) / two
                              yhat = x * exp(-frac23 * x)
                              diff = yhat - y * (x - two)

                              if (diff .gt. zero) then
                                    xnegative = x
                              else
                                    xpositive = x
                              end if

                              if (abs((xpositive - xnegative) / xpositive) .lt. xthresh) then
                                    exit
                              end if
                        end do
                  end if
            end if

            exm13 = exp(-x / three)
            exm23 = exm13**2
            exm = exm13 * exm23
            !
            ! Use chain rule to evaluate
            ! partial derivatives of X variable
            !
            aa = two * pi23 * rho23 * (x - two) / exm23
            bb = three * q * (one - frac23 * x - x / (x - two))
            cc = aa / bb
            dd = -cc * rho / q

            qrho = -gamma / twelve * sigma / rho**2
            qsigma = gamma / twelve / rho

            xrho = frac53 * cc + dd * qrho
            xsigma = dd * qsigma
            xlapl = dd * qlapl
            xtau = dd * qtau
            !
            ! B parameter of the BR hole is given in Eq. 22 in [1].
            ! A and B parameters are treated as functions of X and
            ! RHO. Partial derivatives with respect to X and RHO
            ! are evaluated using the following equations:
            ! X = A(X, RHO) * B(X, RHO)  (see [1])
            ! 1 = dA/dX * B + A * dB/dX
            ! 0 = dA/dRHO * B + A * dB/dRHO
            ! B = X * EXP(-X/3) / (2 * PI**(1/3) * RHO**(1/3))
            !
            brhole_b     = x * exm13 / (two * pi13 * rho13)
            brhole_b_x   = exm13 * (one - frac13 * x) / (two * pi13 * rho13)
            brhole_b_rho = -frac13 * brhole_b / rho
            brhole_a     = x / brhole_b
            brhole_a_x   = (one - brhole_a * brhole_b_x) / brhole_b
            brhole_a_rho = -brhole_a / brhole_b * brhole_b_rho
            call uxtotbr89(brhole_a, brhole_b, exm, uxtot, uxtot_a, uxtot_b)
            call uxlrbr89(brhole_a, brhole_b, omega, uxlr, uxlr_a, uxlr_b)
            uxsr = uxtot - uxlr
            uxsr_a = uxtot_a - uxlr_a
            uxsr_b = uxtot_b - uxlr_b
            uxsr_x = uxsr_a * brhole_a_x + uxsr_b * brhole_b_x
            uxsr_rho = uxsr_a * brhole_a_rho + uxsr_b * brhole_b_rho
            !
            ! Density of exchange energy (per particle of spin sigma)
            !
            eps = frac12 * uxsr
            !
            ! Partial derivatives of exchange
            ! potential (U_{X\sigma}) at a given point in space.
            ! U_{X\sigma} is treated as an explicit function of
            ! X and RHO.
            !
            uxsr_rho   = uxsr_rho + uxsr_x * xrho
            uxsr_sigma = uxsr_x * xsigma
            uxsr_lapl  = uxsr_x * xlapl
            uxsr_tau   = uxsr_x * xtau
            !
            ! Derivatives the sigma part of the exchange energy.
            ! BR definition of E_{X\sigma}:
            ! E_{X\sigma} = 1/2 \int dr \rho_\sigma U_{X\sigma}
            ! (Eq. 9 in [1])
            !
            halfrho = frac12 * rho
            vrho   = frac12 * uxsr + halfrho * uxsr_rho
            vsigma = halfrho * uxsr_sigma
            vlapl  = halfrho * uxsr_lapl
            vtau   = halfrho * uxsr_tau
      end subroutine usrxbr89
      

      subroutine uxtotbr89(a, b, expmx, uxtot, uxtot_a, uxtot_b)
            ! ---------------------------------------------------------------
            ! Exchange potential generated by total (long-range +
            ! short-range) Becke-Roussel exchange hole, see Eq. 23 in [1]. 
            ! ---------------------------------------------------------------
            ! 1. Becke, A.D., Roussel, M.R., Exchange holes in inhomogeneous
            !    systems: A coordinate-space model, Phys. Rev. A, 39,
            !    3761(1989).
            ! 2. Toyoda, M. and Ozaki, T., Exchange functional by a range-
            !    separated exchange hole, Phys. Rev. A 83, 032515 (2011)
            !    [We corrected Eq. 14 in Ref. 2]
            ! ---------------------------------------------------------------
            ! A, B     - Parameters of the Becke-Roussel exchange hole, see
            !            Eq. 17 in [1]
            ! EXPMX    - EXP(-X), X = A * B. Passed as an argument to this
            !            subroutine to avoid multiple evaluation of
            !            exponentials
            ! UXTOT    - U_{X,\sigma} of Eq. 23 in 1. This is the exchange
            !            potential generated by total LR+SR exchange hole
            ! UXTOT_A  - Partial derivative: dUXTOT/dA
            ! UXTOT_B  - Partial derivative: dUXTOT/dB
            !
            real(F64), intent(in)  :: a
            real(F64), intent(in)  :: b
            real(F64), intent(in)  :: expmx
            real(F64), intent(out) :: uxtot
            real(F64), intent(out) :: uxtot_a
            real(F64), intent(out) :: uxtot_b
            
            real(F64) :: x
            real(F64) :: uxtot_x
            
            x = a * b
            uxtot = (-one + (one + frac12 * x) * expmx) / b
            uxtot_x = -expmx * (x + one) / (two * b)
            uxtot_a = uxtot_x * b
            uxtot_b = uxtot_x * a - uxtot / b
      end subroutine uxtotbr89


      subroutine uxlrbr89(a, b, omega, uxlr, uxlr_a, uxlr_b)
            ! ---------------------------------------------------------------
            ! Exchange potential generated by long-range
            ! part of Becke-Roussel exchange hole. The exchange hole is
            ! partitioned by ERF function. A and B parameters of the
            ! Becke-Roussel model [1] are the same as in unmodified
            ! Becke-Roussel functional. (Thus, they differ from the
            ! definition by Toyoda and Ozaki [2] who use range-separated
            ! BR hole in the context of periodic systems.)
            ! 
            ! The electrostatic potential generated by the long-range
            ! BR exchange hole:
            ! -4*Pi * \int \rho_{BR}(a,b,s) * ERF(OMEGA*s) / s * s**2 ds :=  
            ! U_{X\sigma}^{LR} = -omega/Y * ERF(Y) +
            !          omega/(2*Y) * (1-X**2+XY)*ERFC(X-Y)*EXP(X**2-2*X*Y) +
            !          omega/(2*Y) * (-1+X**2+XY)*ERFC(X+Y)*EXP(X**2+2*X*Y)
            !
            ! ---------------------------------------------------------------
            ! 1. Becke, A.D., Roussel, M.R., Exchange holes in inhomogeneous
            !    systems: A coordinate-space model, Phys. Rev. A, 39,
            !    3761(1989).
            ! 2. Toyoda, M. and Ozaki, T., Exchange functional by a range-
            !    separated exchange hole, Phys. Rev. A 83, 032515 (2011)
            !    [We corrected Eq. 14 in Ref. 2]
            ! ---------------------------------------------------------------
            ! A, B   - Parameters of the Becke-Roussel exchange hole, see 
            !          Eq. 17 in [1]
            ! OMEGA  - Range separation parameter of the ERFC function,
            !          \mu in Eq. 11 in [2]
            ! UXLR   - Exchange potential generated by long-range part of BR
            !          hole, Eq. 14 in [2]
            ! UXLR_A - Partial derivative: dUXLR/dA
            ! UXLR_B - Partial derivative: dUXLR/dB
            !
            real(F64), intent(in)  :: a
            real(F64), intent(in)  :: b
            real(F64), intent(in)  :: omega
            real(F64), intent(out) :: uxlr
            real(F64), intent(out) :: uxlr_a
            real(F64), intent(out) :: uxlr_b

            real(F64) :: x, y, x_a, y_b
            real(F64) :: term1, term1_y, erf_y, exp_my2
            real(F64) :: term2, term2_x, term2_y
            real(F64) :: term3, term3_x, term3_y
            real(F64) :: x2, y2, xy, ff
            real(F64) :: exp_t3_x, exp_t3_y, exp_t3
            real(F64) :: prefac, prefac_y
            real(F64) :: prefac2, prefac2_x, prefac2_y
            real(F64) :: erfc_xmy
            real(F64) :: poly1, poly1_x, poly1_y
            real(F64) :: poly2, poly2_x, poly2_y
            real(F64) :: v1, v1_t, v2, v2_t, t1, t2
            real(F64) :: t3, t3_x, t3_y
            real(F64) :: uxlr_x, uxlr_y

            x = a / (two * omega)
            x_a = one / (two * omega)
            y = b * omega
            y_b = omega
            x2 = x**2
            y2 = y**2
            xy = x * y
            exp_my2 = exp(-y2)
            erf_y = erf(y)
            term1 = -omega * erf_y / y
            term1_y = -two * omega / (pi12 * y) * exp_my2 - term1 / y
            prefac = omega / (two * y) * exp_my2
            prefac_y = -prefac / y - two * y * prefac
            poly1 = one - x2 + xy
            poly1_x = -two * x + y
            poly1_y = x
            poly2 = -one + x2 + xy
            poly2_x = two * x + y
            poly2_y = x
            t1 = x - y
            t2 = x + y
            if (t1 > zero) then
                  !
                  ! ERFCEXP subroutine is designed to handle
                  ! the case of large positive argument:
                  ! ERFC(X) * EXP(X**2), X >> 0
                  !
                  call erfcexp(t1, v1, v1_t)
                  term2 = prefac * poly1 * v1 
                  term2_x = prefac * (poly1_x * v1 + poly1 * v1_t)
                  term2_y = prefac_y * poly1 * v1 + prefac * (poly1_y * v1 - poly1 * v1_t)
            else
                  !
                  ! Different strategy must be taken if 
                  ! T1 << 0. In this case, the ERFCEXP
                  ! subroutine would try to evaluate
                  ! ERFC(X-Y) * EXP((X-Y)**2), which goes
                  ! to infinity. Instead, we evaluate
                  ! omega/(2*Y) * ERFC(X-Y) * EXP(X**2-2*X*Y)
                  ! * (1-X**2+X*Y)
                  !
                  t3 = x2 - two * xy
                  t3_x = two * (x - y)
                  t3_y = -two * x
                  erfc_xmy = erfc(t1)
                  !
                  ! EXP(T3) function should be
                  ! close to zero whenever T1<<0
                  ! (large Y)
                  !
                  exp_t3 = exp(t3)
                  exp_t3_x = exp_t3 * t3_x
                  exp_t3_y = exp_t3 * t3_y
                  ff = omega / (two * y)
                  prefac2 = poly1 * ff
                  prefac2_x = poly1_x * ff
                  prefac2_y = poly1_y * ff - prefac2 / y
                  term2 = erfc_xmy * exp_t3 * prefac2
                  term2_x = (-two / pi12 * exp_my2 + erfc_xmy * exp_t3_x) * prefac2 &
                        + erfc_xmy * exp_t3 * prefac2_x
                  term2_y = (two / pi12 * exp_my2 + erfc_xmy * exp_t3_y) * prefac2 &
                        + erfc_xmy * exp_t3 * prefac2_y
            end if
            call erfcexp(t2, v2, v2_t)
            term3 = prefac * poly2 * v2
            term3_x = prefac * (poly2_x * v2 + poly2 * v2_t)
            term3_y = prefac_y * poly2 * v2 + prefac * (poly2_y * v2 + poly2 * v2_t)

            uxlr = term1 + term2 + term3
            uxlr_x = term2_x + term3_x
            uxlr_y = term1_y + term2_y + term3_y
            uxlr_a = uxlr_x * x_a
            uxlr_b = uxlr_y * y_b
      end subroutine uxlrbr89


      pure subroutine erfcexp(x, v, v_x)
            !
            ! V = ERFC(X) * EXP(X**2)
            ! V_X = d/dX (ERFC(X) * EXP(X**2))
            ! 
            real(F64), intent(in)  :: x
            real(F64), intent(out) :: v
            real(F64), intent(out) :: v_x

            real(F64) :: erfc_x, exp_x2, x2
            real(F64) :: s, s2, a, b
            real(F64) :: t1, t2, t3, t4, t5
            !
            ! For X >= 6.0 relative error of the Pade
            ! approximant is below 4.d-15
            !
            real(F64), parameter :: x0 = 6.0d+0
            real(F64), parameter :: a11 = 629.1595402781776d+0
            real(F64), parameter :: a09 = 1469.890174386764d+0
            real(F64), parameter :: a07 = 885.6365987740904d+0
            real(F64), parameter :: a05 = 200.5693969512274d+0
            real(F64), parameter :: a03 = 18.33616146530208d+0
            real(F64), parameter :: a01 = 0.5641895835477563d+0
            real(F64), parameter :: b00 = 1.000000000000000d+0
            real(F64), parameter :: b12 = 162.4218750000000d+0
            real(F64), parameter :: b10 = 1949.062500000000d+0
            real(F64), parameter :: b08 = 3248.437500000000d+0
            real(F64), parameter :: b06 = 1732.500000000000d+0
            real(F64), parameter :: b04 = 371.2500000000000d+0
            real(F64), parameter :: b02 = 33.00000000000000d+0
            
            if (x < x0) then
                  x2 = x**2
                  erfc_x = erfc(x)
                  exp_x2 = exp(x2)
                  v = erfc_x * exp_x2
            else
                  !
                  ! For X >= 6.0 relative error of the Pade
                  ! approximant is below 4.d-15
                  !
                  s = one / x
                  s2 = s**2
                  
                  t1 = a09 + s2 * a11
                  t2 = a07 + s2 * t1
                  t3 = a05 + s2 * t2
                  t4 = a03 + s2 * t3
                  t5 = a01 + s2 * t4
                  a = s * t5

                  t1 = b10 + s2 * b12
                  t2 = b08 + s2 * t1
                  t3 = b06 + s2 * t2
                  t4 = b04 + s2 * t3
                  t5 = b02 + s2 * t4
                  b = b00 + s2 * t5
                  
                  v = a / b
            end if
            !
            ! Derivative is calculated from the definition for both
            ! x < x0 and x > x0 to avoid degradation of accuracy
            ! resulting from differentiation of the Pade approximant
            !
            v_x = -two / pi12 + two * x * v
      end subroutine erfcexp
end module rsbr89
