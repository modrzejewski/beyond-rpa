module br89
      use math_constants

      implicit none
      !
      ! ------------ EXCHANGE PARAMETERS ---------------
      !
      ! Hydrogenic atom, gamma = 1.0 (consistency with Q-Chem, Gaussian 09)
      ! Uniform electron gas, gamma = 0.8
      !
      double precision, parameter, private :: gamma = 1.0d+0
      double precision, parameter, private :: exxcoeff = 0.154d+0
      double precision, parameter, private :: br89coeff = one - exxcoeff
      !
      ! ----------- CORRELATION PARAMETERS -------------
      ! COPP = 2 * 0.63 (pure exchange, non-hybrid)
      !        2 * 0.66 (hybrid, exxcoeff = 0.154)
      !
      double precision, parameter, private :: copp = two * 0.66d+0
      double precision, parameter, private :: cpar = 1.76d+0
      double precision, parameter, private :: iopp = -0.8d+0
      double precision, parameter, private :: ipar = -0.01d+0

contains

      pure subroutine rks_br89(rho, sigma, lapl, tau, eps, vrho, vsigma, vlapl, vtau, npt)
            double precision, dimension(:), intent(in)  :: rho
            double precision, dimension(:), intent(in)  :: sigma
            double precision, dimension(:), intent(in)  :: lapl
            double precision, dimension(:), intent(in)  :: tau
            double precision, dimension(:), intent(out) :: eps
            double precision, dimension(:), intent(out) :: vrho
            double precision, dimension(:), intent(out) :: vsigma
            double precision, dimension(:), intent(out) :: vlapl
            double precision, dimension(:), intent(out) :: vtau
            integer, intent(in)                         :: npt

            integer :: k
            double precision :: rhoa, sigmaa, lapla, taua
            double precision :: u, urho, usigma, ulapl, utau, dipole

            do k = 1, npt
                  rhoa = frac12 * rho(k)
                  sigmaa = frac14 * sigma(k)
                  lapla = frac12 * lapl(k)
                  taua = frac12 * tau(k)

                  call uxbr89(rhoa, sigmaa, lapla, taua, &
                        eps(k), vrho(k), vsigma(k), vlapl(k), vtau(k), &
                        u, urho, usigma, ulapl, utau, dipole)
                  !
                  ! d sigmaa / d sigma = 1/4
                  !
                  vsigma(k) = frac12 * vsigma(k)
            end do
      end subroutine rks_br89


      pure subroutine uks_br89(rho, sigma, lapl, tau, eps, vrho, vsigma, &
            vlapl, vtau, npt)
            
            double precision, dimension(:, :), intent(in)  :: rho
            double precision, dimension(:, :), intent(in)  :: sigma
            double precision, dimension(:, :), intent(in)  :: lapl
            double precision, dimension(:, :), intent(in)  :: tau
            double precision, dimension(:), intent(out)    :: eps
            double precision, dimension(:, :), intent(out) :: vrho
            double precision, dimension(:, :), intent(out) :: vsigma
            double precision, dimension(:, :), intent(out) :: vlapl
            double precision, dimension(:, :), intent(out) :: vtau
            integer, intent(in)                            :: npt

            integer :: k
            double precision :: epsa, epsb
            double precision :: u, urho, usigma, ulapl, utau, dipole

            do k = 1, npt
                  call uxbr89(rho(1, k), sigma(1, k), lapl(1, k), tau(1, k), &
                        epsa, vrho(1, k), vsigma(1, k), vlapl(1, k), vtau(1, k), &
                        u, urho, usigma, ulapl, utau, dipole)

                  call uxbr89(rho(2, k), sigma(3, k), lapl(2, k), tau(2, k), &
                        epsb, vrho(2, k), vsigma(3, k), vlapl(2, k), vtau(2, k), &
                        u, urho, usigma, ulapl, utau, dipole)

                  vsigma(2, k) = zero
                  eps(k) = (epsa * rho(1, k) + epsb * rho(2, k)) / (rho(1, k) + rho(2, k))
            end do
      end subroutine uks_br89


     pure subroutine rks_br89b94hyb(rho, sigma, lapl, tau, &
            eps, vrho, vsigma, vlapl, vtau, npt)

            double precision, dimension(:), intent(in)  :: rho
            double precision, dimension(:), intent(in)  :: sigma
            double precision, dimension(:), intent(in)  :: lapl
            double precision, dimension(:), intent(in)  :: tau
            double precision, dimension(:), intent(out) :: eps
            double precision, dimension(:), intent(out) :: vrho
            double precision, dimension(:), intent(out) :: vsigma
            double precision, dimension(:), intent(out) :: vlapl
            double precision, dimension(:), intent(out) :: vtau
            integer, intent(in)                         :: npt

            integer :: k

            do k = 1, npt
                  call rxcbr89b94hyb(rho(k), sigma(k), lapl(k), tau(k), &
                        eps(k), vrho(k), vsigma(k), vlapl(k), vtau(k))
            end do
      end subroutine rks_br89b94hyb


      pure subroutine uxbr89(rho, sigma, lapl, tau, eps, vrho, vsigma, &
            vlapl, vtau, u, urho, usigma, ulapl, utau, dipole)
            ! ------------------------------------------------------------------
            ! Becke-Roussel 1989 exchange functional. Solution of the nonlinear
            ! equation using numerical fit of Proynov, Gan, Kong.
            ! *** The caller of this subroutine should screen out grid points
            ! with small values of RHO and/or TAU. ***
            ! ------------------------------------------------------------------
            ! 1. Becke, A.D., Roussel, M.R., Exchange holes in inhomogeneous
            !    systems: A coordinate-space model, Phys. Rev. A, 39,
            !    3761(1989).
            ! 2. Proynov, E., Gan, Z., Kong, J., Analytical representation
            !    of the Becke-Roussel exchange functional, Chem. Phys. Lett.
            !    455, 103(2008).
            ! 3. Gori-Giorgi, P., Angyan, J. G., and Savin, A., Charge
            !    density reconstitution from approximate exchange-
            !    correlation holes, Can. J. Chem. 87, 1444 (2009)
            ! 4. Becke, A. D., Johnson, E. R., A density-functional model of
            !    the dispersion interaction, J. Chem. Phys. 123, 154101 (2005)
            ! ------------------------------------------------------------------
            ! RHO    - Electronic density (sigma spin)
            ! SIGMA  - GRAD(1)**2 + GRAD(2)**2 + GRAD(3)**2 (sigma-sigma spin)
            ! LAPL   - Laplacian of the electronic density (sigma spin)
            ! TAU    - Two times kinetic energy density, see def. 13c in [1]
            !          (sigma spin)
            ! EPS    - Density of the BR exchange energy (sigma spin)
            ! VRHO   - RHO derivative of the BR exchange energy (sigma spin)
            ! VSIGMA - SIGMA derivative of the BRexchange energy
            !          (sigma-sigma spin)
            ! VLAPL  - LAPL derivative of the BR exchange energy (sigma spin)
            ! VTAU   - TAU derivative of the BR exchange energy (sigma spin)
            !
            double precision, intent(in)  :: rho
            double precision, intent(in)  :: sigma
            double precision, intent(in)  :: lapl
            double precision, intent(in)  :: tau
            double precision, intent(out) :: eps
            double precision, intent(out) :: vrho
            double precision, intent(out) :: vsigma
            double precision, intent(out) :: vlapl
            double precision, intent(out) :: vtau
            double precision, intent(out) :: u
            double precision, intent(out) :: urho
            double precision, intent(out) :: usigma
            double precision, intent(out) :: ulapl
            double precision, intent(out) :: utau
            double precision, intent(out) :: dipole

            double precision :: x, d, q
            double precision :: exm, exm13, exm23
            double precision :: aa, bb, cc, dd
            double precision :: yx
            double precision :: rho13, rho23, rho53, halfrho
            double precision :: qrho, qsigma
            double precision :: xrho, xsigma, xlapl, xtau
            double precision :: ua, ub, ubx, ubrho, uub, ux
            double precision :: y, y2, y3, y4, y5
            double precision :: p1, p2, g, by
            double precision :: xnew, yhat
            integer          :: i
            !
            ! Partial derivatives of Q variable
            !
            double precision, parameter :: qlapl = frac16
            double precision, parameter :: qtau = -frac13 * gamma
            !
            ! Parameters of numerical fit
            !
            double precision, parameter :: b  = 2.085749716493756d+0

            double precision, parameter :: a1 = 1.5255251812009530d+0
            double precision, parameter :: a2 = 4.576575543602858d-1
            double precision, parameter :: a3 = 4.292036732051034d-1

            double precision, parameter :: b0 = 4.771976183772063d-1
            double precision, parameter :: b1 = -1.7799813494556270d+0
            double precision, parameter :: b2 = 3.8433841862302150d+0
            double precision, parameter :: b3 = -9.5912050880518490d+0
            double precision, parameter :: b4 = 2.1730180285916720d+0
            double precision, parameter :: b5 = -30.425133851603660d+0

            double precision, parameter :: c0 = 7.566445420735584d-1
            double precision, parameter :: c1 = -2.6363977871370960d+0
            double precision, parameter :: c2 = 5.4745159964232880d+0
            double precision, parameter :: c3 = -12.657308127108290d+0
            double precision, parameter :: c4 = 4.1250584725121360d+0
            double precision, parameter :: c5 = -30.425133957163840d+0

            double precision, parameter :: d0 = 4.435009886795587d-5
            double precision, parameter :: d1 = 5.8128653604457910d-1
            double precision, parameter :: d2 = 66.742764515940610d+0
            double precision, parameter :: d3 = 434.26780897229770d+0
            double precision, parameter :: d4 = 824.77657660522390d+0
            double precision, parameter :: d5 = 1657.9652731582120d+0

            double precision, parameter :: e0 = 3.347285060926091d-5
            double precision, parameter :: e1 = 4.7917931023971350d-1
            double precision, parameter :: e2 = 62.392268338574240d+0
            double precision, parameter :: e3 = 463.14816427938120d+0
            double precision, parameter :: e4 = 785.23603501040290d+0
            double precision, parameter :: e5 = 1657.9629682232730d+0
            !
            ! Convergence control of Newton iterations
            !
            integer, parameter :: niter = 100
            double precision, parameter :: thresh = 1.d-14
            double precision, parameter :: xthresh = 1.d-14
            double precision, parameter :: thresh_singular = 0.01d+0
            double precision :: xpositive, xnegative, diff
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
                        diff = abs(yhat - y)
                        if (diff .lt. abs(thresh * y)) exit

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

                              if (abs(xpositive - xnegative) .lt. abs(xthresh * xpositive)) then
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

                              if (abs(xpositive - xnegative) .lt. abs(xthresh * xpositive)) then
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
            ! Exchange potential Ux(r)
            ! ---
            ! Eq. 23 in [1]
            !
            ua = -one + exm * (one + frac12 * x)
            !
            ! B parameter as a function of X: Eq. 22 in [1]
            !
            ub = x * exm13 / (two * pi13 * rho13)
            !
            ! UB is equal to the BR exchange hole dipole
            ! moment, see Refs [3, 4]
            !
            dipole = ub
            u = ua / ub
            !
            ! dB/dX
            !
            ubx = ub * (one / x - frac13)
            !
            ! dB/dRHOS
            !
            ubrho = -frac13 * ub / rho
            ux = -frac12 * exm * (one + x) / ub
            uub = -u / ub
            ux = ux + uub * ubx
            !
            ! Partial derivatives
            ! of exchange potential U(r)
            !
            eps = frac12 * u
            urho = ux * xrho + uub * ubrho
            usigma = ux * xsigma
            ulapl = ux * xlapl
            utau = ux * xtau
            !
            ! Derivatives of integrand F
            !
            halfrho = frac12 * rho
            vrho = frac12 * u + halfrho * urho
            vsigma = halfrho * usigma
            vlapl = halfrho * ulapl
            vtau = halfrho * utau
      end subroutine uxbr89


      pure subroutine br89b94hyb_exx(exx)
            double precision, intent(out) :: exx

            exx = exxcoeff
      end subroutine br89b94hyb_exx
      
      
      pure subroutine rxcbr89b94hyb(rho, sigma, lapl, tau, &
            eps, vrho, vsigma, vlapl, vtau)
            !
            ! -------------------------------------------------------
            ! Exchange-correlation functional using Becke-Roussel 89
            ! exchange and Becke 88 correlation. When mixed with
            ! exact exchange, correlation part should be
            ! reparametrized. This is closed-shell subroutine.
            ! -------------------------------------------------------
            ! 1. Becke, A.D., Roussel, M.R., Exchange holes in
            !    inhomogeneous systems: a coordinate-space model,
            !    Phys. Rev. A, 39, 3761(1989).
            ! 2. Becke, A.D., Correlation energy of an inhomogeneous
            !    electron gas: A coordinate-space model.
            ! 3. Becke, A.D., Thermochemical tests of a
            !    kinetic-energy dependent exchange-correlation
            !    approximation,
            !    Int. J. Quantum Chem. Symp. 28, 625(1994)
            ! 4. Grafenstein, J., Izotov, D., Cremer, D., Avoiding
            !    singularity problems associated with meta-GGA
            !    (generalized gradient approximation) exchange and
            !    correlation functionals containing the kinentic
            !    energy density,
            !    J. Chem. Phys. 127, 214103(2007)
            !
            double precision, intent(in)  :: rho
            double precision, intent(in)  :: sigma
            double precision, intent(in)  :: lapl
            double precision, intent(in)  :: tau
            double precision, intent(out) :: eps
            double precision, intent(out) :: vrho
            double precision, intent(out) :: vsigma
            double precision, intent(out) :: vlapl
            double precision, intent(out) :: vtau

            double precision :: xeps, xvrho, xvsigma, xvlapl, xvtau
            double precision :: u, urho, usigma, ulapl, utau, dipole
            double precision :: rhoa, rhoa2, sigmaa, lapla, taua
            double precision :: z, z2, z4, zu, logz, pref
            double precision :: d, drho, dtau, dsigma
            double precision :: eopp, eoppz, eoppu, eopprho
            double precision :: eoppsigma, eopplapl, eopptau
            double precision :: epar, eparz1, eparz2, eparz
            double precision :: eparu, epard, eparrho, eparsigma
            double precision :: eparlapl, epartau
            double precision :: ta, taexp, ttau
            double precision, parameter :: aconst = 2.d-4

            rhoa = frac12 * rho
            rhoa2 = rhoa**2
            sigmaa = frac14 * sigma
            lapla = frac12 * lapl
            taua = frac12 * tau

            call uxbr89(rhoa, sigmaa, lapla, taua, &
                  xeps, xvrho, xvsigma, xvlapl, xvtau, &
                  u, urho, usigma, ulapl, utau, dipole)
            !
            ! Opposite spin contribution: ab + ba
            !
            z = -copp / u
            z2 = z**2
            zu = -z / u
            logz = log(z + one)
            pref = iopp * rhoa2 * z2
            eopp = pref * (one - logz / z)
            eoppz = (z + one) * logz - z * (two * z + one)
            eoppz = -eoppz * iopp * rhoa2 / (z + one)
            eoppu = eoppz * zu

            eopprho = eopp / rhoa + frac12 * eoppu * urho
            eoppsigma = frac14 * eoppu * usigma
            eopplapl = frac12 * eoppu * ulapl
            eopptau = frac12 * eoppu * utau
            !
            ! Parallel spin contribution: aa + bb
            !
            z = -cpar / u
            zu = -z / u
            z2 = z**2
            z4 = z2 * z2
            !
            ! Regularization of parallel-spin contribution
            ! to the KS matrix. Damping factor from [4]
            !
            taexp = exp(-(taua / aconst)**2)
            ta = one - taexp
            ttau = two / aconst**2 * taua * taexp
            
            d = rhoa * taua - frac14 * sigmaa
            !
            ! d variable should always be nonnegative
            ! due to Schwarz inequality. This constraint may
            ! be violated if kinetic energy density, gradient,
            ! and density are computed approximately.
            !
            d = max(zero, d)
            drho = ta * taua
            dtau = ta * rhoa + ttau * d
            dsigma = -frac14 * ta
            d = ta * d

            pref = ipar * d * z4
            logz = log(one + frac12 * z)
            epar = pref * (one - two / z * logz)
            eparz1 = -two * ipar * z2 * d / (z + two)
            eparz2 = three * logz * (z + two) - z * (two * z + three)
            eparz = eparz1 * eparz2
            eparu = eparz * zu
            pref = ipar * z4
            epard = pref * (one - two / z * logz)

            eparrho = epard * drho + eparu * urho
            eparsigma = frac12 * (epard * dsigma + eparu * usigma)
            eparlapl = eparu * ulapl
            epartau = epard * dtau + eparu * utau
            !
            ! Sum all contributions
            !
            xvsigma = frac12 * xvsigma
            eps    = br89coeff * xeps + (eopp + two * epar) / rho
            vrho   = br89coeff * xvrho + eopprho + eparrho
            vsigma = br89coeff * xvsigma + eoppsigma + eparsigma
            vlapl  = br89coeff * xvlapl + eopplapl + eparlapl
            vtau   = br89coeff * xvtau + eopptau + epartau
      end subroutine rxcbr89b94hyb
end module br89
