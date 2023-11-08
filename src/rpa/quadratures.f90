module quadratures
      use arithmetic
      use math_constants
      use real_linalg
      use io
      use string
      use sort

      implicit none

contains

      function BesselZero(s)
            !
            ! Compute a rational approximation for the s-th positive zero
            ! of the Bessel function J0(x). Use use the most accurate
            ! formula in Table I of Ref. 1.
            !
            ! 1. Lether, F.G. J. Comp. Appl. Math. 67, 167 (1996);
            !    doi: 10.1016/0377-0427(95)00219-7
            !
            real(F64) :: BesselZero
            integer, intent(in) :: s

            real(F64) :: beta, t, R
            real(F64), parameter :: p1 = 1567450796.0_F64/12539606369.0_F64
            real(F64), parameter :: p2 = 8903660.0_F64/2365861.0_F64
            real(F64), parameter :: p3 = 10747040.0_F64/536751.0_F64
            real(F64), parameter :: p4 = 17590991.0_F64/1696654.0_F64
            real(F64), parameter :: q1 = 1.0_F64
            real(F64), parameter :: q2 = 29354255.0_F64/954518.0_F64
            real(F64), parameter :: q3 = 76900001.0_F64/431847.0_F64
            real(F64), parameter :: q4 = 67237052.0_F64/442411.0_F64

            beta = (s - ONE/FOUR) * PI
            t = (ONE/beta)**2
            R = (p1 + p2*t + p3*t**2 + p4*t**3) / (q1 + q2*t + q3*t**2 + q4*t**3)
            BesselZero = beta + R / beta
      end function BesselZero


      function AiZero(m)
            !
            ! Compute an approximation for the m-th Airy function zero.
            ! Use precomputed values for small 1 <= m <= Msmall
            ! Use the asymptotic formula given in Ref. 1 for m > Msmall
            ! The precomputed date were generated using the Mathematica software.
            !
            ! 1. Townsend, A., Trogdon, T., and Olver, S. IMA J. Num. Analysis 36, 337 (2016);
            ! doi: 10.1093/imanum/drv002
            !
            real(F64) :: AiZero
            integer, intent(in) :: m

            integer, parameter :: Msmall = 10
            real(F64), dimension(Msmall) :: AiZeroPrecomp = [ &
                  -2.33810741045976704_F64, &
                  -4.08794944413097060_F64, &
                  -5.52055982809555106_F64, &
                  -6.78670809007175900_F64, &
                  -7.94413358712085312_F64, &
                  -9.02265085334098038_F64, &
                  -10.0401743415580859_F64, &
                  -11.0085243037332629_F64, &
                  -11.9360155632362625_F64, &
                  -12.8287767528657572_F64 &
                  ]
            real(F64), parameter :: p1 = 1.0_F64
            real(F64), parameter :: p2 = 5.0_F64/48.0_F64
            real(F64), parameter :: p3 = -5.0_F64/36.0_F64
            real(F64), parameter :: p4 = 77125.0_F64/82944.0_F64
            real(F64), parameter :: p5 = -108056875.0_F64/6967296.0_F64
            real(F64), parameter :: p6 = 162375596875.0_F64/334430208.0_F64
            real(F64) :: s, t

            if (m <= Msmall) then
                  AiZero = AiZeroPrecomp(m)
            else
                  s = THREE*PI/EIGHT*(FOUR * m - ONE)
                  t = (ONE/s)**2
                  AiZero = -s**(TWO/THREE) * (p1 + p2*t + p3*t**2 + p4*t**3 + p5*t**4 + p6*t**5)
            end if
      end function AiZero


      function GHGuess_Tricomi(k, n)
            !
            ! Compute Tricomi's guess for the node x(k) >= 0 of the Gauss-Hermite
            ! quadrature. Use Lemma 3.1 in Ref. 1.
            !
            ! 1. Townsend, A., Trogdon, T., and Olver, S. IMA J. Num. Analysis 36, 337 (2016);
            ! doi: 10.1093/imanum/drv002
            !
            real(F64) :: GHGuess_Tricomi
            integer, intent(in) :: k
            integer, intent(in) :: n

            real(F64) :: rhs, alpha
            real(F64) :: TauA, TauB, TauK
            real(F64) :: SigmaK, nu, x2
            integer :: kk
            real(F64), parameter :: eps = 1.0E-14_F64

            kk = k - (n/2 + modulo(n, 2))
            alpha = modulo(n, 2) - ONE/TWO
            rhs = (4*(n/2) - 4*kk + 3)*PI / (4*(n/2) + 2*alpha + 2)
            TauA = PI/TWO
            TauB = TauA - (TauA - sin(TauA) - rhs) / (ONE - cos(TauA))
            do while (abs(TauA-TauB) > abs(Taub*eps))
                  TauA = TauB
                  TauB = TauA - (TauA - sin(TauA) - rhs) / (ONE - cos(TauA))
            end do
            TauK = TauB
            SigmaK = cos(TauK/TWO)**2
            nu = 4*(n/2) + 2*alpha + 2
            x2 = nu * SigmaK - ONE/(THREE*nu) * (FIVE/(FOUR*(ONE-SigmaK)**2) - ONE/(ONE-SigmaK) - ONE/FOUR)
            GHGuess_Tricomi = sqrt(x2)
      end function GHGuess_Tricomi
      

      function GHGuess_Gatteschi(k, n)
            !
            ! Compute Gatteschi's guess for the node x(k)>=0 of the Gauss-Hermite quadrature.
            ! Use Lemma 3.2 in Ref. 1.
            !
            ! 1. Townsend, A., Trogdon, T., and Olver, S. IMA J. Num. Analysis 36, 337 (2016);
            ! doi: 10.1093/imanum/drv002
            !
            real(F64) :: GHGuess_Gatteschi
            integer, intent(in) :: k
            integer, intent(in) :: n

            real(F64) :: alpha, nu, t, ak, x2
            real(F64) :: p1, p2, p3, p4, p5
            integer :: kk

            kk = n + 1 - k
            alpha = modulo(n, 2) - ONE/TWO
            nu = 4 * (n/2) + TWO*alpha + TWO
            t = (ONE/nu)**(TWO/THREE)
            ak = AiZero(kk)
            p1 = TWO**(TWO/THREE) * ak
            p2 = TWO**(FOUR/THREE)/FIVE * ak**2
            p3 = 9.0_F64/140.0_F64 - 12.0_F64/175.0_F64 * ak**3
            p4 = (16.0_F64/1575.0_F64 * ak + 92.0_F64/7875.0_F64 * ak**4)*TWO**(TWO/THREE)
            p5 = -(15152.0_F64/3031875.0_F64 * ak**5 + 1088.0_F64/121275.0_F64 * ak**2)*TWO**(ONE/THREE)
            x2 = nu + nu**(ONE/THREE) * (p1 + p2 * t + p3 * t**2 + p4 * t**3 + p5 * t**4)
            GHGuess_Gatteschi = sqrt(x2)
      end function GHGuess_Gatteschi


      function GLGuess_Tricomi(k, n)
            !
            ! Compute Tricomi's guess for the node arccos(x(k)), x(k)>=0 of the Gauss-Legendre quadrature.
            ! Use Eq. 3.4 in Ref. 1
            !
            ! 1. Hale, N. and Townsend, A. SIAM J. Sci. Comput., 35, A652 (2013);
            ! doi: 10.1137/120889873
            !
            real(F64) :: GLGuess_Tricomi
            integer, intent(in) :: k
            integer, intent(in) :: n

            integer :: kk
            real(F64) :: x, phi

            kk = n + 1 - k
            phi = (kk - ONE/FOUR)*PI / (n + ONE/TWO)
            x = (ONE - (n - ONE) / (EIGHT * n**3) - &
                  (39.0_F64 - 28.0_F64/sin(phi)**2) / (384.0_F64 * n**4)) * cos(phi)
            GLGuess_Tricomi = acos(x)
      end function GLGuess_Tricomi


      function GLGuess_Gatteschi(k, n)
            !
            ! Compute Gatteschi's guess for the node arccos(x(k)), x(k)>=0
            ! of the Gauss-Legendre quadrature. Use Eq. 3.7 in Ref. 1.
            !
            ! 1. Hale, N. and Townsend, A. SIAM J. Sci. Comput., 35, A652 (2013);
            !    doi: 10.1137/120889873
            !
            real(F64) :: GLGuess_Gatteschi
            integer, intent(in) :: k
            integer, intent(in) :: n

            integer :: kk
            real(F64) :: psi

            kk = n + 1 - k
            psi = BesselZero(kk) / (n + ONE/TWO)
            GLGuess_Gatteschi = psi + (psi * cos(psi)/sin(psi) - ONE) / (EIGHT * psi * (n + ONE/TWO)**2)
      end function GLGuess_Gatteschi


      subroutine GLGuess(Theta, n)
            !
            ! Compute guess roots of the Gauss-Legendre quadrature.
            ! Use Eq. 3.8 in Ref. 1.
            !
            ! 1. Hale, N. and Townsend, A. SIAM J. Sci. Comput., 35, A652 (2013);
            !    doi: 10.1137/120889873
            !
            real(F64), dimension(:), intent(out) :: Theta
            integer, intent(in) :: n

            integer :: k, l, k0
            real(F64) :: ThetaK

            k0 = n/2 + 1
            k = k0
            if (modulo(n, 2) == 1) then
                  ThetaK = PI/TWO
            else
                  ThetaK = GLGuess_Tricomi(k, n)
            end if
            Theta(1) = ThetaK
            do while (ThetaK >= PI/THREE)
                  k = k + 1
                  ThetaK = GLGuess_Tricomi(k, n)
                  Theta(k-k0+1) = ThetaK                  
            end do
            do l = k, n
                  Theta(l-k0+1) = GLGuess_Gatteschi(l, n)
            end do
      end subroutine GLGuess


      subroutine GHGuess(x, n)
            !
            ! Compute guess roots of the Gauss-Hermite quadrature.
            ! Use the formulas in Section 3.1 in Ref. 1.
            !
            ! 1. Townsend, A., Trogdon, T., and Olver, S. IMA J. Num. Analysis 36, 337 (2016);
            ! doi: 10.1093/imanum/drv002
            !
            real(F64), dimension(:), intent(out) :: x
            integer, intent(in) :: n

            real(F64), parameter :: rho = 0.4985_F64
            integer :: k0_tricomi, k1_tricomi
            integer :: k0_gatteschi, k1_gatteschi
            integer :: k

            k0_tricomi = n / 2 + 1
            k1_tricomi = n / 2 + modulo(n, 2) + floor(rho * n)
            k0_gatteschi = k1_tricomi + 1
            k1_gatteschi = n
            if (modulo(n, 2) == 1) then
                  x(1) = ZERO
            end if
            do k = k0_tricomi+modulo(n, 2), k1_tricomi
                  x(k-k0_tricomi+1) = GHGuess_Tricomi(k, n)
            end do
            do k = k0_gatteschi, k1_gatteschi
                  x(k-k0_tricomi+1) = GHGuess_Gatteschi(k, n)
            end do
      end subroutine GHGuess


      subroutine GLagGuess(x, n)
            !
            ! Compute guess roots of the Gauss-Laguerre quadrature. Diagonalize
            ! the symmetric companion matrix. This code is based on the formulas
            ! used in the NumPy library (v1.13.dev, numpy.polynomial.laguerre.py,
            ! accessed on GitHub on Dec 25th, 2017)
            ! 
            real(F64), dimension(:), intent(out) :: x
            integer, intent(in)                  :: n

            real(F64), dimension(:, :), allocatable :: c
            integer :: k

            allocate(c(n, n))
            c = ZERO
            do k = 1, n - 1
                  c(k, k) = 2 * k - 1
                  c(k+1, k) = -k
            end do
            c(n, n) = 2 * n - 1
            call symmetric_eigenproblem(x, c, n, .false.)
      end subroutine GLagGuess
      
      
      subroutine HermitePolynomial(Ha, Hb, Hc, x, n)
            !
            ! Compute Hermite polynomials of order n for a vector of abscissas x.
            ! To avoid overflows, use the formula for orthonormal polynomials
            ! taken from Numerical Recipes in Fortran 77.
            !
            real(F64), dimension(:), intent(out) :: Ha
            real(F64), dimension(:), intent(out) :: Hb
            real(F64), dimension(:), intent(out) :: Hc
            real(F64), dimension(:), intent(in) :: x
            integer, intent(in) :: n

            integer :: a

            Hb = ZERO
            Ha = ONE/PI**(ONE/FOUR)
            do a = 1, n
                  Hc = Hb
                  Hb = Ha
                  Ha = sqrt(TWO/a) * x * Hb - sqrt((a-ONE)/a) * Hc
            end do
      end subroutine HermitePolynomial


      subroutine LegendrePolynomial(Pa, Pb, Pc, x, n)
            !
            ! Compute Legendre polynomials of order n for a vector of abscissas x.
            !
            real(F64), dimension(:), intent(out) :: Pa
            real(F64), dimension(:), intent(out) :: Pb
            real(F64), dimension(:), intent(out) :: Pc
            real(F64), dimension(:), intent(in) :: x
            integer, intent(in) :: n

            integer :: a

            Pb = ZERO
            Pa = ONE
            do a = 1, n
                  Pc = Pb
                  Pb = Pa
                  Pa = (2*a-ONE)/a * x * Pb - (a-ONE)/a * Pc
            end do
      end subroutine LegendrePolynomial


      subroutine LaguerrePolynomial(Pa, Pb, Pc, x, n)
            !
            ! Compute Laguerre polynomials of order n for a vector of abscissas x.
            !
            real(F64), dimension(:), intent(out) :: Pa
            real(F64), dimension(:), intent(out) :: Pb
            real(F64), dimension(:), intent(out) :: Pc
            real(F64), dimension(:), intent(in) :: x
            integer, intent(in) :: n

            integer :: a

            Pb = ZERO
            Pa = ONE
            do a = 1, n
                  Pc = Pb
                  Pb = Pa
                  Pa = (TWO * a - ONE - x) / a * Pb - (a - ONE) / a * Pc
            end do
      end subroutine LaguerrePolynomial


      subroutine quad_GaussHermite(x, w, converged, n, MaxAbsError)
            !
            ! Compute the non-negative roots x and the corresponding
            ! weights w of the n-point Gauss-Hermite quadrature.
            ! The remaining roots are located symmetrically with respect
            ! to the origin.
            !
            ! The arrays x and w should be of length Floor(n/2)+Mod(n,2).
            ! On exit, x contains the non-negative roots of Hn(x), i.e.,
            ! x(Floor(n/2)+1), x(Floor(n/2)+2), ..., x(n).
            ! When n is odd, the first root stored in x is zero.
            !
            ! For 64-bit reals, the recommended threshold
            ! is MaxAbsError=1.0E-14. We tested that the Newton root finding
            ! converges within roughly 4 iterations for 2 <= n <= 700.
            ! For higher orders, the subroutine HermitePolynomial overflows.
            !
            ! The roots and weights computed with this subroutine
            ! should be used as follows for an even n
            !
            ! int(-inf,inf) f(x) exp(-x**2) dx
            ! = sum(k=1,n/2) w(k) * (f(x(k)) + f(-x(k)))
            !
            real(F64), dimension(:), intent(out) :: x
            real(F64), dimension(:), intent(out) :: w
            logical, intent(out)                 :: converged
            integer, intent(in)                  :: n
            real(F64), intent(in)                :: MaxAbsError

            real(F64), dimension(:), allocatable :: Ha, Hb, Hc
            real(F64), dimension(:), allocatable :: xprev
            integer :: m, l
            integer :: k
            integer, parameter :: maxit = 10

            l = 1 + modulo(n, 2)
            m = n/2 + modulo(n, 2)
            allocate(Ha(m))
            allocate(Hb(m))
            allocate(Hc(m))
            allocate(xprev(m))
            call GHGuess(xprev, n)
            converged = .false.
            if (modulo(n, 2) == 1) then
                  x(1) = ZERO
            end if
            NewtonSteps: do k = 1, maxit
                  call HermitePolynomial(Ha, Hb, Hc, xprev, n)
                  x(l:m) = xprev(l:m) - ONE/sqrt(TWO*n) * Ha(l:m) / Hb(l:m)
                  if (all(abs(x(l:m)-xprev(l:m)) < MaxAbsError)) then
                        converged = .true.
                        exit NewtonSteps
                  else
                        xprev = x
                  end if
            end do NewtonSteps
            !
            ! Quadrature weights. Formula taken from Numerical Recipes
            ! in Fortran 77
            !
            call HermitePolynomial(Ha, Hb, Hc, x, n)
            w = TWO/(TWO*n) / Hb**2
      end subroutine quad_GaussHermite


      subroutine quad_GaussLegendre(x, w, converged, n, MaxAbsError)
            !
            ! Compute the non-negative roots x and the corresponding
            ! weights w of the n-point Gauss-Legendre quadrature (n >= 2).
            ! The remaining roots are located symmetrically with respect
            ! to the origin.
            !
            ! The arrays x and w should be of length Floor(n/2)+Mod(n,2).
            ! On exit, x contains the non-negative roots of Hn(x), i.e.,
            ! x(Floor(n/2)+1), x(Floor(n/2)+2), ..., x(n).
            ! When n is odd, the first root stored in x is zero.
            !
            ! For 64-bit reals, the recommended threshold is MaxAbsError=1.0E-14;
            ! for that threshold, the Newton root finding successfully
            ! converges for 2 <= n <= 440.
            !
            ! The roots and weights computed with this subroutine
            ! should be used as follows for an even n
            !
            ! int(-1,1) f(x) dx = sum(k=1,n/2) w(k) * (f(x(k)) + f(-x(k)))
            !
            ! For numerical stability, the Newton root finding is carried
            ! out in the theta variable (see Ref. 1). MaxAbsError corresponds
            ! to the errors in the theta variable.
            !
            ! 1. Townsend, A., Trogdon, T., and Olver, S. IMA J. Num. Analysis 36, 337 (2016);
            ! doi: 10.1093/imanum/drv002
            !
            !
            real(F64), dimension(:), intent(out) :: x
            real(F64), dimension(:), intent(out) :: w
            logical, intent(out)                 :: converged
            integer, intent(in)                  :: n
            real(F64), intent(in)                :: MaxAbsError

            real(F64), dimension(:), allocatable :: Pa, Pb, Pc, P_Theta
            real(F64), dimension(:), allocatable :: Theta, ThetaPrev, SinTheta
            integer :: m, l
            integer :: k
            integer, parameter :: maxit = 20

            l = 1 + modulo(n, 2)
            m = n/2 + modulo(n, 2)
            allocate(Pa(m))
            allocate(Pb(m))
            allocate(Pc(m))
            allocate(P_Theta(m))
            allocate(Theta(m))
            allocate(ThetaPrev(m))
            allocate(SinTheta(m))
            call GLGuess(ThetaPrev, n)
            converged = .false.
            if (modulo(n, 2) == 1) then
                  x(1) = ZERO
                  Theta(1) = PI/TWO
                  ThetaPrev(1) = PI/TWO
            end if
            NewtonSteps: do k = 1, maxit
                  x(l:m) = cos(ThetaPrev(l:m))
                  call LegendrePolynomial(Pa, Pb, Pc, x, n)
                  !
                  ! Compute d/dTheta Pn(cos(theta)) = -sin
                  !
                  SinTheta(l:m) = sin(ThetaPrev(l:m))
                  P_Theta(l:m) = n * (x(l:m) * Pa(l:m) - Pb(l:m)) / SinTheta(l:m)
                  !
                  ! Newton step for theta angles
                  !
                  Theta(l:m) = ThetaPrev(l:m) - Pa(l:m) / P_Theta(l:m)
                  if (all(abs(Theta(l:m)-ThetaPrev(l:m)) < MaxAbsError)) then
                        converged = .true.
                        exit NewtonSteps
                  else
                        ThetaPrev = Theta
                  end if
            end do NewtonSteps
            !
            ! Quadrature weights. Formula taken from Numerical Recipes
            ! in Fortran 77
            !
            x(l:m) = cos(Theta(l:m))
            call LegendrePolynomial(Pa, Pb, Pc, x, n)
            SinTheta = sin(Theta)
            P_Theta = n * (x * Pa - Pb) / SinTheta
            !
            ! Weights (Eq. 3.9 in Ref. 1)
            !
            w = TWO / P_Theta**2
      end subroutine quad_GaussLegendre


      subroutine quad_GaussLaguerre(x, w, converged, n, MaxAbsError)
            !
            ! Compute the roots x and weights w of the n-point Gauss-Laguerre
            ! quadrature (alpha=0).
            !
            ! The arrays x and w should be of length n.
            !
            ! For 64-bit reals, the recommended threshold
            ! is MaxAbsError=1.0E-14. Works without overflows
            ! for n <= 185.
            !
            ! The roots and weights computed with this subroutine
            ! should be used as follows
            !
            ! int(0,inf) f(x) exp(-x) dx = sum(k=1,n) w(k) * f(x(k))
            !
            real(F64), dimension(:), intent(out) :: x
            real(F64), dimension(:), intent(out) :: w
            logical, intent(out)                 :: converged
            integer, intent(in)                  :: n
            real(F64), intent(in)                :: MaxAbsError

            real(F64), dimension(:), allocatable :: La, Lb, Lc
            real(F64), dimension(:), allocatable :: xprev
            integer :: k
            integer, parameter :: maxit = 10

            allocate(La(n))
            allocate(Lb(n))
            allocate(Lc(n))
            allocate(xprev(n))
            call GLagGuess(xprev, n)
            converged = .false.
            NewtonSteps: do k = 1, maxit
                  call LaguerrePolynomial(La, Lb, Lc, xprev, n)
                  !
                  ! Lc(x) <- d/dx La(x)
                  !
                  Lc = n * (La - Lb) / xprev
                  x = xprev - La / Lc
                  if (all(abs(x-xprev) < MaxAbsError)) then
                        converged = .true.
                        exit NewtonSteps
                  else
                        xprev = x
                  end if
            end do NewtonSteps
            call LaguerrePolynomial(La, Lb, Lc, x, n)
            !
            ! Lc(x) <- d/dx La(x)
            !
            Lc = n * (La - Lb) / x
            !
            ! Quadrature weights. Formulas taken from
            ! Weisstein, Eric W. "Laguerre-Gauss Quadrature."
            ! in MathWorld--A Wolfram Web Resource
            !
            w = -ONE/n / Lb / Lc
      end subroutine quad_GaussLaguerre


      subroutine quad_CasimirPolder(x, w, converged, n, MaxAbsError, alpha)
            !
            ! Generate a modified Gauss-Legendre quadrature for Casimir-Polder type
            ! integrals over imaginary frequencies. The conventional variable
            ! of the Gauss-Legendre quadrature is changed to cover the semi-infinite
            ! interval:
            !
            ! xCP(k) = x0 * (ONE + xGL(k)) / (ONE - xGL(k))
            ! wCP(k) = TWO * wGL(k) * x0 / (ONE - xGL(k))**2
            !
            ! This transformation is used in Ref. 1 for the direct RPA correlation
            ! energy and in the SAPT program of Szalewicz et al. [2]. The recommended
            ! value of the scaling parameter is x0=0.5 (see Appendix C of Ref. 1).
            !
            ! The arrays x and w should be of length n. The points and weights
            ! computed with this subroutine should be used as follows (for n>=2):
            !
            ! Int(0, Inf) Pi(u) du = Sum(k=1,n) w(k) * Pi(x(k))
            !
            ! For 64-bit reals, the recommended threshold for the Gauss-Legendre
            ! root finding is MaxAbsError=1.0E-14; the Newton solver successfully
            ! converges for 2 <= n <= 440.
            !
            ! 1. Ren, X., Rinke, P., Blum, V., Wieferink, J., Tkatchenko, A.,
            !    Sanfilippo, A., Reuter, K., and Scheffler, M.,
            !    New J. Phys. 14, 053020 (2012); doi: 10.1088/1367-2630/14/5/053020
            !
            ! 2. SAPT2016: "An Ab Initio Program for Many-Body Symmetry-Adapted
            !    Perturbation Theory Calculations of Intermolecular
            !    Interaction Energies" by R. Bukowski, W. Cencek, P. Jankowski,
            !    B. Jeziorski, M. Jeziorska, T. Korona, S. A. Kucharski, V. F. Lotrich,
            !    M. P. Metz, A. J. Misquitta, R. Moszynski, K. Patkowski, R. Podeszwa,
            !    F. Rob, S. Rybak, K. Szalewicz, H. L. Williams, R. J. Wheatley,
            !    P. E. S. Wormer, and P. S. Żuchowski.
            !
            real(F64), dimension(:), intent(out) :: x
            real(F64), dimension(:), intent(out) :: w
            logical, intent(out)                 :: converged
            integer, intent(in)                  :: n
            real(F64), intent(in)                :: MaxAbsError
            real(F64), optional, intent(in)      :: alpha

            integer :: nGL
            integer :: idx, k
            real(F64) :: t
            real(F64), dimension(:), allocatable :: xGL, wGL
            real(F64), parameter :: x0def = 0.5_F64
            real(F64) :: x0

            nGL = n / 2 + modulo(n, 2)
            allocate(xGL(nGL))
            allocate(wGL(nGL))
            if (present(alpha)) then
                  x0 = alpha
            else
                  x0 = x0def
            end if
            
            call quad_GaussLegendre(xGL, wGL, converged, n, MaxAbsError)

            do k = nGL, 1, -1
                  t = -xGL(k)
                  idx = nGL - k + 1
                  x(idx) = x0 * (ONE + t) / (ONE - t)
                  w(idx) = TWO * wGL(k) * x0 / (ONE - t)**2
            end do

            do k = 1 + modulo(n, 2), nGL
                  t = xGL(k)
                  idx = n / 2 + k
                  x(idx) = x0 * (ONE + t) / (ONE - t)
                  w(idx) = TWO * wGL(k) * x0 / (ONE - t)**2
            end do
      end subroutine quad_CasimirPolder


      subroutine quad_MiniMax_1_x(x, w, a, b, n)
            !
            ! Compute 1<=n<=63 points and weights of the minimax-optimimal quadrature for the exponential
            ! sum approximation of 1/x. The points and weights are rescaled for x in (a, b).
            !
            ! The arrays x and w should be of length n.
            !
            ! The points and weights are read from the precomputed data included in
            ! the supporting materials for Ref. 1.
            !
            ! 1. Takatsuka, A., Ten-no, S., and Hackbusch, W. J. Chem. Phys. 129, 044112 (2008);
            !    doi: 10.1063/1.2958921
            !
            !
            real(F64), dimension(:), intent(out) :: x
            real(F64), dimension(:), intent(out) :: w
            real(F64), intent(in)                :: a
            real(F64), intent(in)                :: b
            integer, intent(in)                  :: n

            integer, parameter :: Nmin = 1
            integer, parameter :: Nmax = 63
            real(F64), dimension(63), parameter :: Rmin = [ &
                  2.0E00_F64, & ! 1
                  2.0E00_F64, & ! 2
                  2.0E00_F64, & ! 3
                  2.0E00_F64, & ! 4
                  2.0E00_F64, & ! 5
                  2.0E00_F64, & ! 6
                  2.0E00_F64, & ! 7
                  3.0E00_F64, & ! 8
                  3.0E00_F64, & ! 9
                  3.0E00_F64, & ! 10
                  4.0E00_F64, & ! 11
                  5.0E00_F64, & ! 12
                  7.0E00_F64, & ! 13
                  9.0E00_F64, & ! 14
                  1.0E01_F64, & ! 15
                  2.0E01_F64, & ! 16
                  2.0E01_F64, & ! 17
                  3.0E01_F64, & ! 18
                  3.0E01_F64, & ! 19
                  4.0E01_F64, & ! 20
                  6.0E01_F64, & ! 21
                  8.0E01_F64, & ! 22
                  1.0E02_F64, & ! 23
                  2.0E02_F64, & ! 24
                  2.0E02_F64, & ! 25
                  3.0E02_F64, & ! 26
                  4.0E02_F64, & ! 27
                  5.0E02_F64, & ! 28
                  7.0E02_F64, & ! 29
                  9.0E02_F64, & ! 30
                  1.0E03_F64, & ! 31
                  2.0E03_F64, & ! 32
                  2.0E03_F64, & ! 33
                  3.0E03_F64, & ! 34
                  5.0E03_F64, & ! 35
                  7.0E03_F64, & ! 36
                  9.0E03_F64, & ! 37
                  1.0E04_F64, & ! 38
                  2.0E04_F64, & ! 39
                  2.0E04_F64, & ! 40
                  3.0E04_F64, & ! 41
                  5.0E04_F64, & ! 42
                  9.0E04_F64, & ! 43
                  1.0E05_F64, & ! 44
                  2.0E05_F64, & ! 45
                  2.0E05_F64, & ! 46
                  3.0E05_F64, & ! 47
                  4.0E05_F64, & ! 48
                  7.0E05_F64, & ! 49
                  1.0E06_F64, & ! 50
                  2.0E06_F64, & ! 51
                  3.0E06_F64, & ! 52
                  4.0E06_F64, & ! 53
                  6.0E06_F64, & ! 54
                  8.0E06_F64, & ! 55
                  1.0E07_F64, & ! 56
                  9.0E12_F64, & ! 57
                  2.0E13_F64, & ! 58
                  2.0E13_F64, & ! 59
                  3.0E13_F64, & ! 60
                  3.0E13_F64, & ! 61
                  5.0E13_F64, & ! 62
                  6.0E13_F64 &  ! 63
            ]
            real(F64), dimension(63), parameter :: Rmax = [ &
                  1.0E01_F64, & ! 1
                  5.0E01_F64, & ! 2
                  2.0E02_F64, & ! 3
                  5.0E02_F64, & ! 4
                  2.0E03_F64, & ! 5
                  3.0E03_F64, & ! 6
                  7.0E03_F64, & ! 7
                  2.0E04_F64, & ! 8
                  3.0E04_F64, & ! 9
                  6.0E04_F64, & ! 10
                  2.0E05_F64, & ! 11
                  3.0E05_F64, & ! 12
                  4.0E05_F64, & ! 13
                  7.0E05_F64, & ! 14
                  2.0E06_F64, & ! 15
                  3.0E06_F64, & ! 16
                  4.0E06_F64, & ! 17
                  6.0E06_F64, & ! 18
                  1.0E07_F64, & ! 19
                  2.0E07_F64, & ! 20
                  3.0E07_F64, & ! 21
                  4.0E07_F64, & ! 22
                  7.0E07_F64, & ! 23
                  1.0E08_F64, & ! 24
                  2.0E08_F64, & ! 25
                  3.0E08_F64, & ! 26
                  4.0E08_F64, & ! 27
                  6.0E08_F64, & ! 28
                  9.0E08_F64, & ! 29
                  2.0E09_F64, & ! 30
                  2.0E09_F64, & ! 31
                  3.0E09_F64, & ! 32
                  4.0E09_F64, & ! 33
                  6.0E09_F64, & ! 34
                  9.0E09_F64, & ! 35
                  2.0E10_F64, & ! 36
                  2.0E10_F64, & ! 37
                  3.0E10_F64, & ! 38
                  4.0E10_F64, & ! 39
                  5.0E10_F64, & ! 40
                  7.0E10_F64, & ! 41
                  1.0E11_F64, & ! 42
                  2.0E11_F64, & ! 43
                  2.0E11_F64, & ! 44
                  3.0E11_F64, & ! 45
                  4.0E11_F64, & ! 46
                  5.0E11_F64, & ! 47
                  7.0E11_F64, & ! 48
                  1.0E12_F64, & ! 49
                  2.0E12_F64, & ! 50
                  2.0E12_F64, & ! 51
                  3.0E12_F64, & ! 52
                  4.0E12_F64, & ! 53
                  5.0E12_F64, & ! 54
                  6.0E12_F64, & ! 55
                  8.0E12_F64, & ! 56
                  2.0E13_F64, & ! 57
                  2.0E13_F64, & ! 58
                  2.0E13_F64, & ! 59
                  3.0E13_F64, & ! 60
                  4.0E13_F64, & ! 61
                  5.0E13_F64, & ! 62
                  6.0E13_F64 &  ! 63
                  ]
            integer :: p0, p1, q0, q1, p, q
            real(F64) :: r
            integer :: k, u
            character(:), allocatable :: data_dir, file_name
                  
            data_dir = ROOTDIR // "minimax-quad" // DIRSEP
            r = min(b / a, Rmax(n))
            r = max(r, Rmin(n))
            q0 = int(log10(r))
            q1 = ceiling(log10(r))
            p0 = nint(r/(TEN**q0))
            p1 = nint(r/(TEN**q1))
            if (p0 < 10) then
                  p = p0
                  q = q0
            else
                  p = p1
                  q = q1
            end if
            if (n <= 9) then
                  file_name = data_dir // "1_xk0" // str(n) // "." // str(p) // "_" // str(q)
            else
                  file_name = data_dir // "1_xk" // str(n) // "." // str(p) // "_" // str(q)
            end if
            u = io_text_open(file_name, "OLD")
            do k = 1, n
                  read(u, *) w(k)
            end do
            do k = 1, n
                  read(u, *) x(k)
            end do
            w = w / a
            x = x / a
            close(u)
      end subroutine quad_MiniMax_1_x


      subroutine quad_ExpSum_Complex(x, w, n)
            real(F64), dimension(:), intent(out) :: x
            real(F64), dimension(:), intent(out) :: w
            integer, intent(in)                  :: n
            !
            ! Geneneralized Gaussian quadrature for the integral Int(0,Inf) exp(-Lambda * t) dt,
            ! where Lambda=a+ib is a complex number:
            !
            ! 1. Yarvin, N. and Rokhlin, V. SIAM J. Sci. Comput. 20, 699 (1998);
            !    doi: 10.1137/S1064827596310779
            !
            ! The following code and code description is taken from the supplementary materials
            ! for Ref. 1.
            !
            ! This routine returns a set of Gaussian nodes and weights for
            ! integrating the functions exp(lambda*x)dx over the range x=0 to x=infinity.
            ! They work for complex lambda=a+ib about where a is in [1,4] and b is
            ! in [-4,4], except that the left hand two corners of that region have a
            ! square of size 1 cut out.  That is to say, the range over which
            ! the quadratures work is the interior of the following curve:
            ! 
            !      y=4__        ___
            !      y=3__      _|   |
            !                |     |
            !                |     |
            !             .  |     |	  (scale: 2 characters per x increment,
            !            0   |     |		  1 character per y increment)
            !                |     |
            !      y=-3__    |_    |
            !      y=-4__      |___|
            !
            !		   ^   ^
            !		 ^ |   x=4
            !		 | x=2
            !		x=1
            !
            !
            ! Input arguments:
            !	n - number of weights and nodes in the quadrature.  This must
            !	  be an integer in the range [2,33].
            !
            ! Output arguments:
            !	w - weights
            !	x - nodes
            !
            !
            !
            ! The approximate accuracies of the quadratures are:
            !	n	maximum error
            !        2     0.14479E+00
            !        3     0.60327E-01
            !        4     0.23052E-01
            !        5     0.88523E-02
            !        6     0.31391E-02
            !        7     0.10731E-02
            !        8     0.36463E-03
            !        9     0.12118E-03
            !       10     0.39871E-04
            !       11     0.12928E-04
            !       12     0.42110E-05
            !       13     0.13889E-05
            !       14     0.45621E-06
            !       15     0.14892E-06
            !       16     0.48360E-07
            !       17     0.15634E-07
            !       18     0.50116E-08
            !       19     0.15958E-08
            !       20     0.50620E-09
            !       21     0.16432E-09
            !       22     0.56513E-10
            !       23     0.19359E-10
            !       24     0.66963E-11
            !       25     0.23161E-11
            !       26     0.80125E-12
            !       27     0.28168E-12
            !       28     0.98263E-13
            !       29     0.35897E-13
            !       30     0.13474E-13
            !       31     0.52420E-14
            !       32     0.27262E-14
            !       33     0.20800E-14
            !
            !
            if (n <  2 .or. n > 33) then
                  call msg("Invalid value of n=" // str(n) // " in ExpSum_Complex", MSG_ERROR)
                  stop
            end if

            if (n == 2) then
                  x(1)=0.28400379135928016088827529264657275D+00
                  x(2)=0.12645433108861215274260451898337497D+01
                  w(1)=0.67826513188267390457361186085436258D+00
                  w(2)=0.12205244765610583138649539703008742D+01
            end if
            if (n == 3) then
                  x(1)=0.21771410353739076147718243415712852D+00
                  x(2)=0.10143043578871001441108110148376246D+01
                  x(3)=0.22470583084556003482552945578073719D+01
                  w(1)=0.52516865473744813510352887551739658D+00
                  w(2)=0.10482915850964959874302859661557135D+01
                  w(3)=0.13963009790143132109940084461995307D+01
            end if
            if (n == 4) then
                  x(1)=0.17653126345387967269995508578113458D+00
                  x(2)=0.83686445629685270881562147275574361D+00
                  x(3)=0.19015583225195693015720412128965454D+01
                  x(4)=0.32847826031130255776828532887011246D+01
                  w(1)=0.43512643931510641072223166910627628D+00
                  w(2)=0.87187843122230405766946755640662913D+00
                  w(3)=0.12451439750466389437602538399153821D+01
                  w(4)=0.15123479751939728202007633278333474D+01
            end if
            if (n == 5) then
                  x(1)=0.14608412754905099018074728792853670D+00
                  x(2)=0.71123236850115983064537916734445083D+00
                  x(3)=0.16248526631083160966003299550470372D+01
                  x(4)=0.28521260230834528754641487722285642D+01
                  x(5)=0.43348283018685763948284606712067026D+01
                  w(1)=0.36521610341416504282608013485294144D+00
                  w(2)=0.74727827262862776617091744634535145D+00
                  w(3)=0.10776602105161139183774088981303829D+01
                  w(4)=0.13689030656537346904960307053441120D+01
                  w(5)=0.15955104962294020813105847773147893D+01
            end if
            if (n == 6) then
                  x(1)=0.12545997485969736459851271599833943D+00
                  x(2)=0.61977832952374764668582057193228451D+00
                  x(3)=0.14234620883410192635053665755050922D+01
                  x(4)=0.25051406321659361879750864988124445D+01
                  x(5)=0.38471715902518535858230412212550473D+01
                  x(6)=0.54090491131043198643744530871493041D+01
                  w(1)=0.31524181615190498738163561531084736D+00
                  w(2)=0.65932610698635801889814585410288000D+00
                  w(3)=0.94408311492216381985567186225544171D+00
                  w(4)=0.12174552613417637292937615599239218D+01
                  w(5)=0.14617675969096581700034169256822552D+01
                  w(6)=0.16645472922657597665897574194750730D+01
            end if
            if (n == 7) then
                  x(1)=0.10992716182389423141244323845085228D+00
                  x(2)=0.54916941623367809272064043335190333D+00
                  x(3)=0.12714168272863413847990217026790778D+01
                  x(4)=0.22395230564743011944348488230112537D+01
                  x(5)=0.34468363300051984597444733701620796D+01
                  x(6)=0.48776660687723027164005141280897328D+01
                  x(7)=0.65026079151870522276023772180399467D+01
                  w(1)=0.27755962243083718138584499693260481D+00
                  w(2)=0.59006125627449070569122821533187619D+00
                  w(3)=0.84786105271593626343362741237464767D+00
                  w(4)=0.10885109461642135792862950868246700D+01
                  w(5)=0.13237320658690068607713707201579958D+01
                  w(6)=0.15348388775139320381566003008763619D+01
                  w(7)=0.17190483490279343109762475560799919D+01
            end if
            if (n == 8) then
                  x(1)=0.97438183268937133847010994847718142D-01
                  x(2)=0.49176607123457506973473096002021480D+00
                  x(3)=0.11476300941975544504289907867097191D+01
                  x(4)=0.20281861013918041213785923144102174D+01
                  x(5)=0.31216620892446888472060205905739318D+01
                  x(6)=0.44256141601586141431903633052701893D+01
                  x(7)=0.59254210853241738442177191682147770D+01
                  x(8)=0.76012890093531286856489888650833282D+01
                  w(1)=0.24699442798088208409044921135866515D+00
                  w(2)=0.53286495525271603946247023717331217D+00
                  w(3)=0.77248536039577146871981223697149924D+00
                  w(4)=0.98701872460412025404621770230453970D+00
                  w(5)=0.12001216947212026496065793702776852D+01
                  w(6)=0.14054235866128710033869760977294544D+01
                  w(7)=0.15927961044860063481620994851299595D+01
                  w(8)=0.17651983538768606235745492367781858D+01
            end if
            if (n == 9) then
                  x(1)=0.87474984508955801751244408489549187D-01
                  x(2)=0.44478467408488852207352307456138819D+00
                  x(3)=0.10446564552615584893840294730533044D+01
                  x(4)=0.18538065355548559439298750209323576D+01
                  x(5)=0.28550455295215975283601919689978094D+01
                  x(6)=0.40471786030085814158614568900275930D+01
                  x(7)=0.54269484893261465491724330030682318D+01
                  x(8)=0.69824362739872290139508019432475585D+01
                  x(9)=0.87013652814807534769859100256117263D+01
                  w(1)=0.22229286524410940506065439022861162D+00
                  w(2)=0.48521426852287542384648053341754055D+00
                  w(3)=0.70893800199798183281507572878186225D+00
                  w(4)=0.90638376714631425433495187112844579D+00
                  w(5)=0.10963732370699054418983183330099565D+01
                  w(6)=0.12875247922176986805705064906236802D+01
                  w(7)=0.14701265960009182379393129268885054D+01
                  w(8)=0.16406518624184869812437892550971517D+01
                  w(9)=0.18048587598066448985461812704582306D+01
            end if
            if (n == 10) then
                  x(1)=0.79400973700479499935128555213140699D-01
                  x(2)=0.40599675027044619366888546847196557D+00
                  x(3)=0.95860548270566904353161214898108566D+00
                  x(4)=0.17076338623411163593261713260463299D+01
                  x(5)=0.26342522431201571140581812351214416D+01
                  x(6)=0.37330678114549471128034521693621167D+01
                  x(7)=0.50056635563091919404907878992765512D+01
                  x(8)=0.64476147019688302576592933529310484D+01
                  x(9)=0.80499560865687448969045153873638195D+01
                  x(10)=0.98062704155363721864842086211412233D+01
                  w(1)=0.20213268247442060867475698387441100D+00
                  w(2)=0.44529201310708537641318658274237358D+00
                  w(3)=0.65492570079022382976521136655750443D+00
                  w(4)=0.83991908942837778794988649284982981D+00
                  w(5)=0.10125227869573986025903611515489316D+01
                  w(6)=0.11856981580215336911473355206365302D+01
                  w(7)=0.13587490932348737761895434846098690D+01
                  w(8)=0.15237759923040749432879745844369563D+01
                  w(9)=0.16815303253729589933261188748846843D+01
                  w(10)=0.18393633495134452397615160066179384D+01
            end if
            if (n == 11) then
                  x(1)=0.72689324399385037097872824273507216D-01
                  x(2)=0.37336128260409716833921520499910945D+00
                  x(3)=0.88562468369440848396851952374585357D+00
                  x(4)=0.15831791022025034769510559624752389D+01
                  x(5)=0.24475601549243929203331367645526780D+01
                  x(6)=0.34698258287300001775674215215588736D+01
                  x(7)=0.46504130655260912838137931649886943D+01
                  x(8)=0.59903088291066920394337369310156789D+01
                  x(9)=0.74846241589323666281830997736808818D+01
                  x(10)=0.91273629085034314854560291083642264D+01
                  x(11)=0.10916876937832505863471593992813460D+02
                  w(1)=0.18530050691138275432673359348856793D+00
                  w(2)=0.41134378129627275265725674380076503D+00
                  w(3)=0.60869815274447726492811080996538265D+00
                  w(4)=0.78332864623548391364896227404387792D+00
                  w(5)=0.94389918807093236678876151853160878D+00
                  w(6)=0.11009043232488180274077220087830672D+01
                  w(7)=0.12605831740960009943959899475152926D+01
                  w(8)=0.14183341569022032847112339088780031D+01
                  w(9)=0.15694455635770408719533273586345508D+01
                  w(10)=0.17174060769932164021682340691187517D+01
                  w(11)=0.18710210331422505315794151421144834D+01
            end if
            if (n == 12) then
                  x(1)=0.66994411912954399266462826687055003D-01
                  x(2)=0.34537503794600023481823901892860981D+00
                  x(3)=0.82257258430427345663908744047359146D+00
                  x(4)=0.14753121341549897541705943598105731D+01
                  x(5)=0.22860682798258656789545631900224901D+01
                  x(6)=0.32442726026154080477435820678864225D+01
                  x(7)=0.43472736748543734916316057165225641D+01
                  x(8)=0.55973570531547623347938151741131699D+01
                  x(9)=0.69941805398477678436489449145468874D+01
                  x(10)=0.85332353022561075120247540012249897D+01
                  x(11)=0.10211273598987784163140737054301730D+02
                  x(12)=0.12030524931071580749873332521509414D+02
                  w(1)=0.17096466979866617586331205920891166D+00
                  w(2)=0.38195507132063508136701951060645550D+00
                  w(3)=0.56845141134719540115481410091916432D+00
                  w(4)=0.73410798399352608479096129353072473D+00
                  w(5)=0.88561153443954390298263662619972270D+00
                  w(6)=0.10303948865919596241011400527274960D+01
                  w(7)=0.11761612049611493153061304963319423D+01
                  w(8)=0.13239824889753918130489575396444883D+01
                  w(9)=0.14688518753694151965307747157433663D+01
                  w(10)=0.16088776060489329430316596559880729D+01
                  w(11)=0.17491527657957810933944246730836319D+01
                  w(12)=0.18995985004161037200223049177934035D+01
            end if
            if (n == 13) then
                  x(1)=0.62119693070159074422709210375040452D-01
                  x(2)=0.32118676611812397934069366513854056D+00
                  x(3)=0.76762568139470299864868646938280718D+00
                  x(4)=0.13808949364684408286595353657052046D+01
                  x(5)=0.21445743734065183622051041740298335D+01
                  x(6)=0.30476611768121012890503424973213805D+01
                  x(7)=0.40850755693474119616017740788060636D+01
                  x(8)=0.52575729083536686623452191688129054D+01
                  x(9)=0.65672785179441870136872262236369232D+01
                  x(10)=0.80128462616126357643724473974015799D+01
                  x(11)=0.95906612907300500153196765186206983D+01
                  x(12)=0.11299899736529734616253536212567457D+02
                  x(13)=0.13145980518264992898749960871483026D+02
                  w(1)=0.15865532518687772980332685592524480D+00
                  w(2)=0.35631602398985521768749977134089523D+00
                  w(3)=0.53303132196533887714469115349271523D+00
                  w(4)=0.69076228860464931459726007078358637D+00
                  w(5)=0.83475751094892659241040204256760549D+00
                  w(6)=0.97055215950873951348245306468840243D+00
                  w(7)=0.11044915129330217441287933693042996D+01
                  w(8)=0.12409633782036594859030602194010784D+01
                  w(9)=0.13782018881304393510557491702372750D+01
                  w(10)=0.15122800773161414893974207273425287D+01
                  w(11)=0.16433654681581861462254088876185450D+01
                  w(12)=0.17774675229864803263904449043077568D+01
                  w(13)=0.19252312582372090082182229315757728D+01
            end if
            if (n == 14) then
                  x(1)=0.57909043501391694219877057098197548D-01
                  x(2)=0.30013086620426351279255408214523370D+00
                  x(3)=0.71943038024503768085759167227884496D+00
                  x(4)=0.12976742251766228241312672597040307D+01
                  x(5)=0.20195914853898833916752857047828697D+01
                  x(6)=0.28742585992411360897865334299760932D+01
                  x(7)=0.38552825772379780914574864438805122D+01
                  x(8)=0.49612503594228076445761808894306160D+01
                  x(9)=0.61942819840403783345893845760599020D+01
                  x(10)=0.75556705913906267021242773890181421D+01
                  x(11)=0.90436113776864682163681416909240603D+01
                  x(12)=0.10655613500796125174141022981787183D+02
                  x(13)=0.12392954905163175125458054056460573D+02
                  x(14)=0.14263753984930607242129748207750863D+02
                  w(1)=0.14799786862588675084008614112970022D+00
                  w(2)=0.33381916050126295771813574427880860D+00
                  w(3)=0.50165769090607950607092286522660315D+00
                  w(4)=0.65226978279541425358799430924742765D+00
                  w(5)=0.78974613788957943327499307762553144D+00
                  w(6)=0.91850814216788952224675214340676597D+00
                  w(7)=0.10433213411066669319008238933822047D+01
                  w(8)=0.11690595179825893186486204086184971D+01
                  w(9)=0.12972661252428974873914698667728202D+01
                  w(10)=0.14251687588361022954150176715433814D+01
                  w(11)=0.15502595131936793871628115663001705D+01
                  w(12)=0.16741082943179844433233192851292284D+01
                  w(13)=0.18033281192590612545128113189957689D+01
                  w(14)=0.19494975352410525723029649017013502D+01
            end if
            if (n == 15) then
                  x(1)=0.54229083896808971833993033603821362D-01
                  x(2)=0.28161576653275820692177436146343680D+00
                  x(3)=0.67678231403467504982805711508082846D+00
                  x(4)=0.12237116458762810252445022360753283D+01
                  x(5)=0.19082703298778887533208320546113438D+01
                  x(6)=0.27198311008216285402935838048261345D+01
                  x(7)=0.36514155344635414422706228700790004D+01
                  x(8)=0.46999456979757571739682817897905781D+01
                  x(9)=0.58661961224166012199876979137598626D+01
                  x(10)=0.71524452832322891809330784417917577D+01
                  x(11)=0.85591355189112492593310125645104104D+01
                  x(12)=0.10084439185276764241437115289667603D+02
                  x(13)=0.11727030388958038331917831633797993D+02
                  x(14)=0.13489986440426905872952097096685077D+02
                  x(15)=0.15383732505248489244377090092820310D+02
                  w(1)=0.13866673981848914610630858805984886D+00
                  w(2)=0.31391042428328432251280578563595022D+00
                  w(3)=0.47366263602939466471098822630114249D+00
                  w(4)=0.61782055809150986454428619961528374D+00
                  w(5)=0.74952698931195217034884617067526235D+00
                  w(6)=0.87242724616636070244335177954275326D+00
                  w(7)=0.99022121579002295587725547398526526D+00
                  w(8)=0.11070107462233999025915669564419759D+01
                  w(9)=0.12259397096084818337508294847518186D+01
                  w(10)=0.13466404371624598961063893476671220D+01
                  w(11)=0.14663903026455235491484444109983351D+01
                  w(12)=0.15839673220555647685307629392243889D+01
                  w(13)=0.17018711898335938050197241639475319D+01
                  w(14)=0.18271200347428789001695102648544384D+01
                  w(15)=0.19721600270765192745323550394050288D+01
            end if
            if (n == 16) then
                  x(1)=0.50985651676060703651538328313720038D-01
                  x(2)=0.26521044170119000279421506792606483D+00
                  x(3)=0.63877523197814057194831650521526435D+00
                  x(4)=0.11575210696261955883231582181815506D+01
                  x(5)=0.18084194569861212306495891989304084D+01
                  x(6)=0.25812535628463792003577074861261020D+01
                  x(7)=0.34688375582342867575415476321407594D+01
                  x(8)=0.44670816002182330969272754435069641D+01
                  x(9)=0.55752269484049928085382726995880302D+01
                  x(10)=0.67950948336753630583904201325509319D+01
                  x(11)=0.81285324158592800794661776023331542D+01
                  x(12)=0.95753440067592315377471327981505964D+01
                  x(13)=0.11133967001554672444157889374470906D+02
                  x(14)=0.12804196003878287405397056876695693D+02
                  x(15)=0.14590645231278099027047339768309646D+02
                  x(16)=0.16505707680646148788973069377822442D+02
                  w(1)=0.13042997567403945298242325657326993D+00
                  w(2)=0.29617079732513148873183821456411636D+00
                  w(3)=0.44851876604376184944450589661269086D+00
                  w(4)=0.58677848054269017278709561636156953D+00
                  w(5)=0.71331490632911520500252692710575776D+00
                  w(6)=0.83115583049433048884840792306050173D+00
                  w(7)=0.94332332670457374311018023093132337D+00
                  w(8)=0.10530465654290454721288481339340199D+01
                  w(9)=0.11635980151835412179751770788751378D+01
                  w(10)=0.12764875588622434869414871611208197D+01
                  w(11)=0.13903417411114882623872055443209606D+01
                  w(12)=0.15029785005709018170479777972198986D+01
                  w(13)=0.16142090491898998439599193293273266D+01
                  w(14)=0.17271419059134570866090433742114100D+01
                  w(15)=0.18490423387442161310085827289258515D+01
                  w(16)=0.19928836723099874470314913854407848D+01
            end if
            if (n == 17) then
                  x(1)=0.48107012020670752121769350734664249D-01
                  x(2)=0.25058487577619272446075114759235837D+00
                  x(3)=0.60471372473597283743388297976018980D+00
                  x(4)=0.10979569045692173244336726903297849D+01
                  x(5)=0.17183387185623772630862623592449699D+01
                  x(6)=0.24561167581495938408852379276046714D+01
                  x(7)=0.33040983407714680196270972838720529D+01
                  x(8)=0.42576381825486778811056494776374731D+01
                  x(9)=0.53147924200070716335712437574546475D+01
                  x(10)=0.64762816706856718288338898365779951D+01
                  x(11)=0.77441920702444067851479967654406230D+01
                  x(12)=0.91197616909124722383724820967988099D+01
                  x(13)=0.10602430199899893256072006939602047D+02
                  x(14)=0.12191077345418507534490973307437702D+02
                  x(15)=0.13886543651828657185210629430302360D+02
                  x(16)=0.15694781513039847251761918684728063D+02
                  x(17)=0.17629930642343106751933885249641636D+02
                  w(1)=0.12311046348926950785374329120768897D+00
                  w(2)=0.28027830315791539633440984983934018D+00
                  w(3)=0.42581976817476960710048833388042233D+00
                  w(4)=0.55864620933417860493933563588512112D+00
                  w(5)=0.68049097825977032840593846858243907D+00
                  w(6)=0.79386749512018295122379526958055994D+00
                  w(7)=0.90131842168989334839435495169130275D+00
                  w(8)=0.10054340488545246818587468347931646D+01
                  w(9)=0.11090164993412379537057747947685240D+01
                  w(10)=0.12143539751558416431254385165942150D+01
                  w(11)=0.13216916194915539488924617556226297D+01
                  w(12)=0.14293314619338509313907012790005670D+01
                  w(13)=0.15357812859350491635623672788403867D+01
                  w(14)=0.16416331994572298084312982301929260D+01
                  w(15)=0.17504083222167288305730109017163385D+01
                  w(16)=0.18696084724114359553642836881544405D+01
                  w(17)=0.20130389886655532301908465281181916D+01
            end if
            if (n == 18) then
                  x(1)=0.45533073219773336444890038944893378D-01
                  x(2)=0.23745778335040697840445639663338898D+00
                  x(3)=0.57399938994284233730455685699620636D+00
                  x(4)=0.10440338017800521481189312203969013D+01
                  x(5)=0.16365715584209938910316214519203340D+01
                  x(6)=0.23423746162687875263057215758719723D+01
                  x(7)=0.31543593059253376280839536404995909D+01
                  x(8)=0.40675954383630240748323620817278467D+01
                  x(9)=0.50793575818224314039783910315601119D+01
                  x(10)=0.61892520677380631545272210943135920D+01
                  x(11)=0.73987969031358436207515960327830965D+01
                  x(12)=0.87099003615923011483854942074665315D+01
                  x(13)=0.10123246124451790900149802513186744D+02
                  x(14)=0.11638154011743809293962323013675386D+02
                  x(15)=0.13254035562232463216933127783483221D+02
                  x(16)=0.14972681454338681406861221579860395D+02
                  x(17)=0.16801205030198956356335361021883061D+02
                  x(18)=0.18755296147480344612730580051400105D+02
                  w(1)=0.11655892811355466871747267066574411D+00
                  w(2)=0.26595412921805333667094457469561300D+00
                  w(3)=0.40521404439515930802698747998159399D+00
                  w(4)=0.53299859134072126806927153848131418D+00
                  w(5)=0.65053258609468098694541969326408319D+00
                  w(6)=0.75989397351227331116106538620305714D+00
                  w(7)=0.86325309881763939195134969129472034D+00
                  w(8)=0.96276117053626496304634966351595382D+00
                  w(9)=0.10607020958261615731558288708676289D+01
                  w(10)=0.11593682445074525017649722297781179D+01
                  w(11)=0.12600714520074810238703030802153782D+01
                  w(12)=0.13622517666759905114600229834580331D+01
                  w(13)=0.14643007187957510658536142263076220D+01
                  w(14)=0.15653840912531639037504590541306505D+01
                  w(15)=0.16666595551096578554788087093371974D+01
                  w(16)=0.17719247717127016312362057053580998D+01
                  w(17)=0.18888723570132958673638652431193830D+01
                  w(18)=0.20320582346550418157092787782410262D+01
            end if
            if (n == 19) then
                  x(1)=0.43220250279628530815487307378696978D-01
                  x(2)=0.22562395288760778674718196530134861D+00
                  x(3)=0.54619611331431217989594503046845463D+00
                  x(4)=0.99504172556562056406736358926370684D+00
                  x(5)=0.15620852371227691941516787899196801D+01
                  x(6)=0.22386012673301934634087759682133931D+01
                  x(7)=0.30176845945696000711085834160959902D+01
                  x(8)=0.38942804556883601610410971230801558D+01
                  x(9)=0.48651802499128500572926215609566973D+01
                  x(10)=0.59291198490168377351312756491500828D+01
                  x(11)=0.70867490726658203807157668532529225D+01
                  x(12)=0.83398837986035032517266471114891809D+01
                  x(13)=0.96900582701762944097420302780140858D+01
                  x(14)=0.11137537953505256500560304866799352D+02
                  x(15)=0.12681697824082715765172772944434109D+02
                  x(16)=0.14322507764889973620173285098947752D+02
                  x(17)=0.16062607590575486041561547538780266D+02
                  x(18)=0.17910115551812816704431259487600764D+02
                  x(19)=0.19882082987740513113034329515448199D+02
                  w(1)=0.11066685862053051707777092458940780D+00
                  w(2)=0.25299404779766825790778136002824837D+00
                  w(3)=0.38644823656262751618843423652096770D+00
                  w(4)=0.50953977959177794525739735758844912D+00
                  w(5)=0.62308677311996883551937401452953083D+00
                  w(6)=0.72879487434611708721541161149737147D+00
                  w(7)=0.82852973108399006103255959497857421D+00
                  w(8)=0.92412518305673356831752181075111229D+00
                  w(9)=0.10174623162626507341671061254069400D+01
                  w(10)=0.11105372285268910967880115553223280D+01
                  w(11)=0.12050516644272337291096229198436339D+01
                  w(12)=0.13014926370005664508186556062764095D+01
                  w(13)=0.13988932589435307246936811269533007D+01
                  w(14)=0.14959365249883425026319413939280027D+01
                  w(15)=0.15923489331277872212591990142501704D+01
                  w(16)=0.16896924516397163517553548269074768D+01
                  w(17)=0.17919495545711698561077130219947603D+01
                  w(18)=0.19069426469947997082978996228190329D+01
                  w(19)=0.20496843593348750273766159908594914D+01
            end if
            if (n == 20) then
                  x(1)=0.41131145979463386563317347431338603D-01
                  x(2)=0.21490516397774288353799825975834284D+00
                  x(3)=0.52092083198272767066218058456774427D+00
                  x(4)=0.95035468517020596213634146400136930D+00
                  x(5)=0.14939737079831026678358588120190988D+01
                  x(6)=0.21435619898940762402453306133549331D+01
                  x(7)=0.28924358672478006619790718484600154D+01
                  x(8)=0.37355158999226733818623311699847489D+01
                  x(9)=0.46693038617598502339739178697375668D+01
                  x(10)=0.56919268541268061000365608099563725D+01
                  x(11)=0.68032075877294582070478383035190185D+01
                  x(12)=0.80044144066048127417332900180624237D+01
                  x(13)=0.92973319076349607999422613445454614D+01
                  x(14)=0.10683075590303826771162258740600231D+02
                  x(15)=0.12161641253867360454327958317080870D+02
                  x(16)=0.13732576098803153477824611375246961D+02
                  x(17)=0.15396404521335401367390523365471607D+02
                  x(18)=0.17156529423687294090780363939002853D+02
                  x(19)=0.19021965093257286837096227613453981D+02
                  x(20)=0.21011017753694340868051634752884260D+02
                  w(1)=0.10534081028713577046829368619639459D+00
                  w(2)=0.24121831593176975126076047660471298D+00
                  w(3)=0.36929669630117515202018036332385558D+00
                  w(4)=0.48800943138710742154081686084921938D+00
                  w(5)=0.59785192124813823580255355392898972D+00
                  w(6)=0.70021034637605386507799095552479841D+00
                  w(7)=0.79669158544619625254229811464919455D+00
                  w(8)=0.88888469090464998681049682377008542D+00
                  w(9)=0.97837703935487040373503661695532016D+00
                  w(10)=0.10668421937400309900455436073225190D+01
                  w(11)=0.11559472379780992009509357583560295D+01
                  w(12)=0.12467866814357869952308737732108864D+01
                  w(13)=0.13392427298091470649174665162803186D+01
                  w(14)=0.14322316046478213795120326874656668D+01
                  w(15)=0.15248005225309463949067548001329786D+01
                  w(16)=0.16171303844482442599480973420698701D+01
                  w(17)=0.17110756119621504536694818177134340D+01
                  w(18)=0.18107596169260235985879011283009647D+01
                  w(19)=0.19241741563932821836038462124379517D+01
                  w(20)=0.20671065493976104719325915503688588D+01
            end if
            if (n == 21) then
                  x(1)=0.39231835495405765245329619240997090D-01
                  x(2)=0.20513669580082400809742045505267913D+00
                  x(3)=0.49781132761293333477649939441589670D+00
                  x(4)=0.90936910568165892928483537064607606D+00
                  x(5)=0.14313512688990022196727892243947815D+01
                  x(6)=0.20560411336943076520949701105515441D+01
                  x(7)=0.27770039728827152282077457571621891D+01
                  x(8)=0.35892008905608141896885407007824024D+01
                  x(9)=0.44889683666480695493640277544737757D+01
                  x(10)=0.54740248072097374374590105862753369D+01
                  x(11)=0.65435348308084990441993139598258947D+01
                  x(12)=0.76980826512319820791745984637735114D+01
                  x(13)=0.89392203676229044491064364918863524D+01
                  x(14)=0.10268518754168112221980130958717001D+02
                  x(15)=0.11686720243136313798331238025980962D+02
                  x(16)=0.13193688886483761492991663412888043D+02
                  x(17)=0.14789219217463962758282986633497464D+02
                  x(18)=0.16474367419196419696639543403221878D+02
                  x(19)=0.18253217383533290583085905327422731D+02
                  x(20)=0.20135576136926152074548431758416368D+02
                  x(21)=0.22140888008895162422006471318030039D+02
                  w(1)=0.10049557217382083690817602953782780D+00
                  w(2)=0.23045710840888092017145647996110056D+00
                  w(3)=0.35353811896363258564290475552476962D+00
                  w(4)=0.46814642272990682373732647316708128D+00
                  w(5)=0.57452369534149621045591994818430199D+00
                  w(6)=0.67378240232840506020676174957810187D+00
                  w(7)=0.76730417511497336893137749501990770D+00
                  w(8)=0.85647904995406955056883922176155255D+00
                  w(9)=0.94267427515951055440042937784451717D+00
                  w(10)=0.10272988151154589909473420248668237D+01
                  w(11)=0.11118256559012401388556377994955521D+01
                  w(12)=0.11975462527745574903694542508862191D+01
                  w(13)=0.12850071730117115591355636784474993D+01
                  w(14)=0.13737141797597972162784604492017691D+01
                  w(15)=0.14626517149138238560170391542233554D+01
                  w(16)=0.15512296401679348253317990806205878D+01
                  w(17)=0.16399830728302631153551083725390524D+01
                  w(18)=0.17309754251171498230604765801441385D+01
                  w(19)=0.18284332360867918484525978483403041D+01
                  w(20)=0.19404959144483666984090053263161463D+01
                  w(21)=0.20836154685779856993473088815485869D+01
            end if
            if (n == 22) then
                  x(1)=0.37499754871412546942985547582399111D-01
                  x(2)=0.19620928532428017648055039948983065D+00
                  x(3)=0.47662866556437418915622619747436312D+00
                  x(4)=0.87168922540890408920962866176619749D+00
                  x(5)=0.13736386626517326891648461805722128D+01
                  x(6)=0.19752418255662746592531735612532033D+01
                  x(7)=0.26703285300938148412686716591317627D+01
                  x(8)=0.34539442550851998096423544426365398D+01
                  x(9)=0.43223433485754464740708670571870801D+01
                  x(10)=0.52729757588266515389889125533543856D+01
                  x(11)=0.63045233820212270345107404433681971D+01
                  x(12)=0.74169394125917444157810415169129801D+01
                  x(13)=0.86112903858967682793235902138256409D+01
                  x(14)=0.98891726798762183235625662041139742D+01
                  x(15)=0.11251867523947022038241860488371928D+02
                  x(16)=0.12699826020708944603790153305482890D+02
                  x(17)=0.14232889197894917179266799138441693D+02
                  x(18)=0.15851139445684266622415622295668829D+02
                  x(19)=0.17556131261924068835460296681919908D+02
                  x(20)=0.19352554961587416888229270900621041D+02
                  x(21)=0.21250918822049454369927592908384870D+02
                  x(22)=0.23271691230992161582163670277211602D+02
                  w(1)=0.96074499371815824425885381409930341D-01
                  w(2)=0.22059823921412056645649125696273726D+00
                  w(3)=0.33902806306853662632179845038021636D+00
                  w(4)=0.44978144573917012059705583041361238D+00
                  w(5)=0.55290310111289047591279907181621049D+00
                  w(6)=0.64927294940402920964983234204087496D+00
                  w(7)=0.74007517607139681195037226508505605D+00
                  w(8)=0.82653234801265403419755430048535156D+00
                  w(9)=0.90984015040711898712840651332891629D+00
                  w(10)=0.99120383833887574376019041828860029D+00
                  w(11)=0.10718854523628164014027370428095824D+01
                  w(12)=0.11531344214899622821205727215049963D+01
                  w(13)=0.12358494430531713978129156352051872D+01
                  w(14)=0.13201383982392127213151867024197071D+01
                  w(15)=0.14053223756174048234923080645010634D+01
                  w(16)=0.14905532841399735505953038863208751D+01
                  w(17)=0.15755670652988963042673596393714627D+01
                  w(18)=0.16611699743757687103167862023801271D+01
                  w(19)=0.17495751669156678357649012854934216D+01
                  w(20)=0.18450845327954659084235969147708811D+01
                  w(21)=0.19559521728326226265584823616523439D+01
                  w(22)=0.20990839525024246310174406063982774D+01
            end if
            if (n == 23) then
                  x(1)=0.35914359057909064201449944544034079D-01
                  x(2)=0.18802271056119242774555325229800617D+00
                  x(3)=0.45715237746029467694731802095468798D+00
                  x(4)=0.83695026500547979605094491006698037D+00
                  x(5)=0.13203066066602115765511160544600013D+01
                  x(6)=0.19004457841026798029850879619855638D+01
                  x(7)=0.25714693069716352175480385089356938D+01
                  x(8)=0.33285363714624996998729305076091947D+01
                  x(9)=0.41678758255061526472860160728529625D+01
                  x(10)=0.50867657983855459072698932729879536D+01
                  x(11)=0.60835459361868200694229058927473125D+01
                  x(12)=0.71576598374769762781465212192939541D+01
                  x(13)=0.83096318579256680455037576760938286D+01
                  x(14)=0.95407824961796622991078532633069095D+01
                  x(15)=0.10852600181397785225842895068051684D+02
                  x(16)=0.12246070840062527990660185808802985D+02
                  x(17)=0.13721444318770651151017967278120688D+02
                  x(18)=0.15278619422583551959561635892697386D+02
                  x(19)=0.16917984983041044173884692096723926D+02
                  x(20)=0.18641558707619128475911052639213640D+02
                  x(21)=0.20454577085324164049877396180424784D+02
                  x(22)=0.22368186409442745694424009366529772D+02
                  x(23)=0.24403819332185689171083046225326428D+02
                  w(1)=0.92025900773330597368135983405848295D-01
                  w(2)=0.21153801397555643469817423057085742D+00
                  w(3)=0.32563259508222629880689293696136938D+00
                  w(4)=0.43276034488651822877043194983375999D+00
                  w(5)=0.53281449256437037743096375688194171D+00
                  w(6)=0.62647826315225585554035576075847767D+00
                  w(7)=0.71476210826083875442630240488873544D+00
                  w(8)=0.79874398437153186134991256964997734D+00
                  w(9)=0.87948107770045238502687690538475818D+00
                  w(10)=0.95802123524825390727915832920063650D+00
                  w(11)=0.10354461379243300812037832881748886D+01
                  w(12)=0.11128734291042470215337659058959711D+01
                  w(13)=0.11913019853988187187338523618086512D+01
                  w(14)=0.12712599492051827770163236332688963D+01
                  w(15)=0.13525440781527954267656393966167787D+01
                  w(16)=0.14344319194967134634433238598692222D+01
                  w(17)=0.15162837933837522365050165902932037D+01
                  w(18)=0.15981132037713311737246085163398593D+01
                  w(19)=0.16809327597820754813793946325406953D+01
                  w(20)=0.17670662116236613872497039698765443D+01
                  w(21)=0.18608816713353107137367156939952434D+01
                  w(22)=0.19707735140979759377931406773673233D+01
                  w(23)=0.21142762088389047123484997968120249D+01
            end if
            if (n == 24) then
                  x(1)=0.34456845156983675620373135326928076D-01
                  x(2)=0.18048429300595764631900494930733653D+00
                  x(3)=0.43917597701736378110684890918646938D+00
                  x(4)=0.80480728480591262392494676557286523D+00
                  x(5)=0.12708526895944610857732861004605862D+01
                  x(6)=0.18309724576007537233342812579165339D+01
                  x(7)=0.24795414024428394745084084482227134D+01
                  x(8)=0.32118520549853558046000040125802370D+01
                  x(9)=0.40241468202015684584868844549866138D+01
                  x(10)=0.49135989676392352423093086389899527D+01
                  x(11)=0.58783089447938829699055132864620608D+01
                  x(12)=0.69173328558834497894062171453553228D+01
                  x(13)=0.80307042725836276375164965384401118D+01
                  x(14)=0.92193290141365679532899802232095246D+01
                  x(15)=0.10484607993376017735875224760999545D+02
                  x(16)=0.11827841249286617538793146116071686D+02
                  x(17)=0.13249754158923357459463249912026200D+02
                  x(18)=0.14750479027136931635124693857280410D+02
                  x(19)=0.16330028816170971986846298456472344D+02
                  x(20)=0.17989101837956887282140943451886261D+02
                  x(21)=0.19730142325229278819343662561331085D+02
                  x(22)=0.21558878586976380253232951097255047D+02
                  x(23)=0.23487036928925167717276489577066804D+02
                  x(24)=0.25536952836347974429277019061259575D+02
                  w(1)=0.88302334756891107780899488885387712D-01
                  w(2)=0.20317937502985412587866652385237555D+00
                  w(3)=0.31322382072559551063791395257331676D+00
                  w(4)=0.41693497914456031139185676828380800D+00
                  w(5)=0.51409072546023629052660897981594456D+00
                  w(6)=0.60520829024608734576895806294337285D+00
                  w(7)=0.69114477491033347235594546840545499D+00
                  w(8)=0.77285073615126111133820044484055815D+00
                  w(9)=0.85126777364772082304767905725370106D+00
                  w(10)=0.92731937946760535772681527891245572D+00
                  w(11)=0.10019426034379106055084927748801785D+01
                  w(12)=0.10761121051984410160893615319737713D+01
                  w(13)=0.11507880390201417910046003283626535D+01
                  w(14)=0.12267058377500815793888531955317579D+01
                  w(15)=0.13040776394672441353026451342610433D+01
                  w(16)=0.13825101007250553415626761127672300D+01
                  w(17)=0.14613293791078211980933379303564492D+01
                  w(18)=0.15401089768921500766948494279629293D+01
                  w(19)=0.16190903883728903749724859243116134D+01
                  w(20)=0.16994423263395919762646652525749572D+01
                  w(21)=0.17835710995278957766397607017359992D+01
                  w(22)=0.18759027884495109644367578803725445D+01
                  w(23)=0.19849747312194892872570489041836367D+01
                  w(24)=0.21289671348433224838680433986639221D+01
            end if
            if (n == 25) then
                  x(1)=0.33112325633819625613083119481162799D-01
                  x(2)=0.17352028801304600470682259456797807D+00
                  x(3)=0.42253412131121919382273632554865128D+00
                  x(4)=0.77498255696301537436908355599277861D+00
                  x(5)=0.12248708994703129997546446848138199D+01
                  x(6)=0.17662709048263180075138808955009382D+01
                  x(7)=0.23938286638991559394521580088970798D+01
                  x(8)=0.31029834400068183355418076087415647D+01
                  x(9)=0.38900191122874615351873741864853955D+01
                  x(10)=0.47520516906366177917389373270717699D+01
                  x(11)=0.56870170076177419440757073565059677D+01
                  x(12)=0.66936842986910137801036433455479235D+01
                  x(13)=0.77716843757696686530431460793210774D+01
                  x(14)=0.89214895735448753186881475438986093D+01
                  x(15)=0.10144226973560876574542757808581838D+02
                  x(16)=0.11441256026731357027473463843332728D+02
                  x(17)=0.12813657999122059429124479407362668D+02
                  x(18)=0.14261948523823582823230110012502475D+02
                  x(19)=0.15786211151294678535802770925159339D+02
                  x(20)=0.17386608947054772029730916621750419D+02
                  x(21)=0.19064142531918023407775293929487328D+02
                  x(22)=0.20821646111629076392695692834639439D+02
                  x(23)=0.22665289605067117694272635838661510D+02
                  x(24)=0.24607321564887424040004839922751082D+02
                  x(25)=0.26670904003418593412264861255182633D+02
                  w(1)=0.84866188123667157362341786048640773D-01
                  w(2)=0.19544464453584830632498205388872552D+00
                  w(3)=0.30169830969848962061050045081550583D+00
                  w(4)=0.40218478086402350129578858878623467D+00
                  w(5)=0.49659562048334723627683744474990876D+00
                  w(6)=0.58530874065211664209056176090016401D+00
                  w(7)=0.66904604984053440959794806666868759D+00
                  w(8)=0.74864450779874963201226999829142898D+00
                  w(9)=0.82494643620151404956368307153892551D+00
                  w(10)=0.89877444496636140873837132417361873D+00
                  w(11)=0.97095045527917140441627719096015866D+00
                  w(12)=0.10423226073824387415420569921508871D+01
                  w(13)=0.11137591194323882197110201979924519D+01
                  w(14)=0.11860465304231339463140421155177808D+01
                  w(15)=0.12596642484125342723938936845012249D+01
                  w(16)=0.13345793925482255580721049787360543D+01
                  w(17)=0.14103080813553000894565038975328721D+01
                  w(18)=0.14862779820727957426470414351247106D+01
                  w(19)=0.15622618547411159349263907028870435D+01
                  w(20)=0.16386850208101025076874491440707468D+01
                  w(21)=0.17168341535042418081864292769561218D+01
                  w(22)=0.17991771388046548438715649631553393D+01
                  w(23)=0.18901867806897214950223143742537457D+01
                  w(24)=0.19985109799722391666451246455002012D+01
                  w(25)=0.21427408503492931494656761559068143D+01
            end if
            if (n == 26) then
                  x(1)=0.31868528127071675607566451636141914D-01
                  x(2)=0.16706965113525610431160315184304567D+00
                  x(3)=0.40708922061450962999633650189544782D+00
                  x(4)=0.74724391750443314148448200832366277D+00
                  x(5)=0.11820207063298251240520388484860097D+01
                  x(6)=0.17058779181706891664665421616889729D+01
                  x(7)=0.23137279219589108316871181363455099D+01
                  x(8)=0.30011654475538275834686717913959588D+01
                  x(9)=0.37645360845463704992331227144958199D+01
                  x(10)=0.46009330238981504454099653744420974D+01
                  x(11)=0.55081816759905061293050633532311900D+01
                  x(12)=0.64848417224447658505285609919081553D+01
                  x(13)=0.75302288341881333630409406079198308D+01
                  x(14)=0.86444263245295647213963851055358835D+01
                  x(15)=0.98282116958133835924486521852636912D+01
                  x(16)=0.11082805246263723247476078620126065D+02
                  x(17)=0.12409449391528177108146410736889796D+02
                  x(18)=0.13809010990338617964313823774782793D+02
                  x(19)=0.15281852196842488289397795802350191D+02
                  x(20)=0.16828063048751907421580240843154672D+02
                  x(21)=0.18447979328091890023093226670636014D+02
                  x(22)=0.20142892818304547980118102983293505D+02
                  x(23)=0.21915996030424219275412981664167717D+02
                  x(24)=0.23773861639507392149696357991067616D+02
                  x(25)=0.25729223070710973927120995765788411D+02
                  x(26)=0.27806033565269779372737994880563713D+02
                  w(1)=0.81686419853244356616740412811377009D-01
                  w(2)=0.18826921095747787912096490913870411D+00
                  w(3)=0.29096929051665234542929784034664207D+00
                  w(4)=0.38840805353481319073632601378014542D+00
                  w(5)=0.48021400960023889175866639412324305D+00
                  w(6)=0.56664891834115013408246578519553558D+00
                  w(7)=0.64831595277502317619065336493636269D+00
                  w(8)=0.72595022578767932437482675928616716D+00
                  w(9)=0.80030690497566825637714944407139598D+00
                  w(10)=0.87212451411581275411333897280767680D+00
                  w(11)=0.94213231286553501166866509172488163D+00
                  w(12)=0.10110733498899777266339428495145965D+01
                  w(13)=0.10797157042821566898880359378944102D+01
                  w(14)=0.11488130158628870164066876826902609D+01
                  w(15)=0.12189686450249156983480345706528275D+01
                  w(16)=0.12904328479748047344805114655646062D+01
                  w(17)=0.13630022957603708298645502103702613D+01
                  w(18)=0.14361771276441191218720715868005568D+01
                  w(19)=0.15095113981299483295070514334346765D+01
                  w(20)=0.15829545912058270808083842745634486D+01
                  w(21)=0.16570786571124597783244648191194369D+01
                  w(22)=0.17332602663998719863689476821779163D+01
                  w(23)=0.18140159819219871196279407060635226D+01
                  w(24)=0.19038652690119789391961982127645325D+01
                  w(25)=0.20115853064778462998762048251232802D+01
                  w(26)=0.21562956232479617656286264360873527D+01
            end if
            if (n == 27) then
                  x(1)=0.30714153459932747446057989658152502D-01
                  x(2)=0.16107591228580310812975197366084252D+00
                  x(3)=0.39271325891923582397602620771295905D+00
                  x(4)=0.72137444051511930194848283916559829D+00
                  x(5)=0.11419834352756013902842406625301919D+01
                  x(6)=0.16493603634580431393546804242430670D+01
                  x(7)=0.22386764876534426176771247719442889D+01
                  x(8)=0.29056869194320249339477046424256039D+01
                  x(9)=0.36468134368803206666219999928708469D+01
                  x(10)=0.44591515021795001957797211126994847D+01
                  x(11)=0.53404556716549223044617777260466418D+01
                  x(12)=0.62891340248374208859929482196171545D+01
                  x(13)=0.73042608902949782125842587589368876D+01
                  x(14)=0.83855956054555937167620405771349934D+01
                  x(15)=0.95335652448819936225852870372235224D+01
                  x(16)=0.10749137317637380107038077153645439D+02
                  x(17)=0.12033530344575561820548838833522482D+02
                  x(18)=0.13387832099257433156343862766846367D+02
                  x(19)=0.14812720575077005801302260852368011D+02
                  x(20)=0.16308458527348279034659454932017628D+02
                  x(21)=0.17875184628920889302473699967043080D+02
                  x(22)=0.19513419845469550071155487016011102D+02
                  x(23)=0.21224734035885138985900687819375970D+02
                  x(24)=0.23012651598141649877606276533279246D+02
                  x(25)=0.24884118558630676165548345756761417D+02
                  x(26)=0.26852326852251004140839897342918664D+02
                  x(27)=0.28942002736761184382368572140029222D+02
                  w(1)=0.78734413745112572309257747366606982D-01
                  w(2)=0.18159297990863012266178939224890543D+00
                  w(3)=0.28095534614086367558900414909963056D+00
                  w(4)=0.37550913777298997097771067042854061D+00
                  w(5)=0.46483803779505084094670635583323851D+00
                  w(6)=0.54910734274794777159593684691415373D+00
                  w(7)=0.62881622794078451184457446376921300D+00
                  w(8)=0.70460827788614583776839119437598483D+00
                  w(9)=0.77716098206725819583761915147422313D+00
                  w(10)=0.84714102239555521458893180301144926D+00
                  w(11)=0.91520175864633585172470452362350863D+00
                  w(12)=0.98200026227438221241569209575441227D+00
                  w(13)=0.10482134506430578442759092915446316D+01
                  w(14)=0.11145291018565507172921263674872239D+01
                  w(15)=0.11815764087597510688170879024051861D+01
                  w(16)=0.12497777295605177452093490010561164D+01
                  w(17)=0.13191947403208197449581293995612050D+01
                  w(18)=0.13895218869071112063409999135324003D+01
                  w(19)=0.14602937522879333727662111176635650D+01
                  w(20)=0.15311966721786843472492305176322324D+01
                  w(21)=0.16023314933470080592970434482770812D+01
                  w(22)=0.16743866451767669535891853561315047D+01
                  w(23)=0.17488091367297186167202648357751295D+01
                  w(24)=0.18281570876560826639307226988302804D+01
                  w(25)=0.19169991544570315981503140373473167D+01
                  w(26)=0.20242620335911908171122063845433387D+01
                  w(27)=0.21697393777457121215855549499166171D+01
            end if
            if (n == 28) then
                  x(1)=0.29639938944116973288947198168708967D-01
                  x(2)=0.15549263059151681562948879145367590D+00
                  x(3)=0.37930055094030209231760800221697395D+00
                  x(4)=0.69719457827937021144799186512467105D+00
                  x(5)=0.11044952398228416711490912641552919D+01
                  x(6)=0.15963603051623994311675384122432674D+01
                  x(7)=0.21682109795668010026191123762228292D+01
                  x(8)=0.28159648815465413449080788411447410D+01
                  x(9)=0.35361303501524203707471901444912120D+01
                  x(10)=0.43258237584665801716072505095153489D+01
                  x(11)=0.51827582519090394337007079729618911D+01
                  x(12)=0.61052339911984875101135093340805539D+01
                  x(13)=0.70921428836831544196529173287002748D+01
                  x(14)=0.81429849067494339718858438226790120D+01
                  x(15)=0.92578739260296481310544700449421018D+01
                  x(16)=0.10437484697062940279396673795170932D+02
                  x(17)=0.11682880600212392486834938705723342D+02
                  x(18)=0.12995214531995254539198187552821426D+02
                  x(19)=0.14375409571801324686300790008509746D+02
                  x(20)=0.15823989880242162591363098296266098D+02
                  x(21)=0.17341165576068506937049153404959312D+02
                  x(22)=0.18927153581844557790364195713478426D+02
                  x(23)=0.20582662896584541726443138668433232D+02
                  x(24)=0.22309521901007604327919784000023857D+02
                  x(25)=0.24111553297166766600808141893442458D+02
                  x(26)=0.25996035821874600906996641324454793D+02
                  x(27)=0.27976573340121689811932736726743769D+02
                  x(28)=0.30078597062512304623298714682478950D+02
                  w(1)=0.75986690744677089014568663714956065D-01
                  w(2)=0.17536634982966723574497746643032331D+00
                  w(3)=0.27158889494709209892985972155407373D+00
                  w(4)=0.36340858574607780580181492344729872D+00
                  w(5)=0.45037863603492914011714360811314766D+00
                  w(6)=0.53258462568936066842448596956564738D+00
                  w(7)=0.61043490882007419889164010891368512D+00
                  w(8)=0.68449077964010353870409110400716677D+00
                  w(9)=0.75535981148678173624286200650143058D+00
                  w(10)=0.82364605546665955763197432645983035D+00
                  w(11)=0.88993954012210407637353817555831776D+00
                  w(12)=0.95482693608831652563124712316526978D+00
                  w(13)=0.10189072984303173783257668461547534D+01
                  w(14)=0.10827964351251543962109975123394414D+01
                  w(15)=0.11470968789928207839990921990582785D+01
                  w(16)=0.12123071544791901429248749599135597D+01
                  w(17)=0.12786825978106659660749842362504650D+01
                  w(18)=0.13461422031992972889377608875615000D+01
                  w(19)=0.14143337426347047147569963696241800D+01
                  w(20)=0.14828563337776464200638498747513559D+01
                  w(21)=0.15515230588506349108221229187839404D+01
                  w(22)=0.16205609613146071581088506581062936D+01
                  w(23)=0.16907489493893908818873122582004707D+01
                  w(24)=0.17635860170290635583004633069005209D+01
                  w(25)=0.18416611592901820226543844655347613D+01
                  w(26)=0.19295828891736381242738721993678866D+01
                  w(27)=0.20363812191490803455355120071111879D+01
                  w(28)=0.21821526471450971956160040877873075D+01
            end if
            if (n == 29) then
                  x(1)=0.28638376730037411610956365795418739D-01
                  x(2)=0.15028214086507606938014242863004911D+00
                  x(3)=0.36676539100842457164143158598768938D+00
                  x(4)=0.67455900068434832176427311210888532D+00
                  x(5)=0.10693434194491980016579651693100754D+01
                  x(6)=0.15465908734518338953693116542353656D+01
                  x(7)=0.21019625723385190518266023307098649D+01
                  x(8)=0.27315387021266122875891896692453236D+01
                  x(9)=0.34319214825216267377260094798951527D+01
                  x(10)=0.42002621334405089999299555103483528D+01
                  x(11)=0.50342544440643402683410621689273635D+01
                  x(12)=0.59321235371539119125140893265558878D+01
                  x(13)=0.68926245981549958988228342132593889D+01
                  x(14)=0.79150539212400424760617565331240540D+01
                  x(15)=0.89992614744426416545258833317750177D+01
                  x(16)=0.10145636244080720881594540058486561D+02
                  x(17)=0.11355016697397726987025346989384271D+02
                  x(18)=0.12628488352899444018707400929012527D+02
                  x(19)=0.13967099653446499970797997929344978D+02
                  x(20)=0.15371615848808733984478149904454131D+02
                  x(21)=0.16842443769478561185036126705680970D+02
                  x(22)=0.18379778906233784107651799237860237D+02
                  x(23)=0.19983937164206222847292227007955177D+02
                  x(24)=0.21655815989652042108152882702542215D+02
                  x(25)=0.23397488828436789294549004195726099D+02
                  x(26)=0.25213057594647730083823799974089566D+02
                  x(27)=0.27110115301861286669176418091105122D+02
                  x(28)=0.29102665972709652770901822971147392D+02
                  x(29)=0.31216849198488345343156387568492455D+02
                  w(1)=0.73424215920058035265185561989511094D-01
                  w(2)=0.16954907466603919560269100163491331D+00
                  w(3)=0.26281520528923410232204899491494075D+00
                  w(4)=0.35204252292866171960057004340908603D+00
                  w(5)=0.43676507602714146656199637138257060D+00
                  w(6)=0.51700297655826703291667737963464588D+00
                  w(7)=0.59308552954426387406010222701993987D+00
                  w(8)=0.66550008729667540261294500227383357D+00
                  w(9)=0.73479085161536815168334656637314986D+00
                  w(10)=0.80150630341543239589701015374420167D+00
                  w(11)=0.86618253181284857230043138258684382D+00
                  w(12)=0.92934797757640264478046322129480170D+00
                  w(13)=0.99153658509897763513609394039286697D+00
                  w(14)=0.10532971767846119395228289154896239D+01
                  w(15)=0.11151841320826767417732850562186067D+01
                  w(16)=0.11777085853806667258629744733214463D+01
                  w(17)=0.12412382990332989944605687379949247D+01
                  w(18)=0.13058842192135969891247893724731110D+01
                  w(19)=0.13714673007851600266723417410514008D+01
                  w(20)=0.14376303189034047482793476620812803D+01
                  w(21)=0.15040524142175184067558229544294055D+01
                  w(22)=0.15706628285133193041447963659220408D+01
                  w(23)=0.16377936906902593402498399959727806D+01
                  w(24)=0.17062964643419086250012516236784719D+01
                  w(25)=0.17777126452419118641537682176919486D+01
                  w(26)=0.18546606322046457277528900465461106D+01
                  w(27)=0.19417964890371207241820727791674528D+01
                  w(28)=0.20483000440126803568924196417905083D+01
                  w(29)=0.21949056062431502595838678226586526D+01
            end if
            if (n == 30) then
                  x(1)=0.27701018168124800637783062763693035D-01
                  x(2)=0.14540162239688844126618653323265795D+00
                  x(3)=0.35500881006691987041976979969004637D+00
                  x(4)=0.65329693424197565217361949476094526D+00
                  x(5)=0.10362739802787561646229072960226419D+01
                  x(6)=0.14997051468659891638736598329447580D+01
                  x(7)=0.20394815486339304073581887814983554D+01
                  x(8)=0.26518447433219965605140725585310344D+01
                  x(9)=0.33334960439889137043299949104192700D+01
                  x(10)=0.40816321230424028724838423325357293D+01
                  x(11)=0.48939441507500684096225683664795727D+01
                  x(12)=0.57686070568858669112007869455223106D+01
                  x(13)=0.67042743108376382900128070285261325D+01
                  x(14)=0.77000835841007903150298695723746534D+01
                  x(15)=0.87556689687851037891369035501171557D+01
                  x(16)=0.98711634491394068824111586609071557D+01
                  x(17)=0.11047159579399002995449388959388227D+02
                  x(18)=0.12284588202587182864422125999960086D+02
                  x(19)=0.13584503182103078639409186241736777D+02
                  x(20)=0.14947830491781883623508919964686153D+02
                  x(21)=0.16375195945321934740460880572211401D+02
                  x(22)=0.17866924815345871309626714038880311D+02
                  x(23)=0.19423228555743472229450353912825729D+02
                  x(24)=0.21044535629953283618923339129988309D+02
                  x(25)=0.22731928804580661853300943349142165D+02
                  x(26)=0.24487713052454684435685987533306582D+02
                  x(27)=0.26316251674072902956508599526206884D+02
                  x(28)=0.28225432692507291966419767751875992D+02
                  x(29)=0.30229638904911552812778694341879592D+02
                  x(30)=0.32355693268160340691373820551826050D+02
                  w(1)=0.71025513967316715573251290902670361D-01
                  w(2)=0.16409482256844163299224118755653538D+00
                  w(3)=0.25456938132325724028813710596557435D+00
                  w(4)=0.34133295105478308495134394858292092D+00
                  w(5)=0.42390918581435197159994449471687752D+00
                  w(6)=0.50226467092496838336796913479756924D+00
                  w(7)=0.57666000923206910750628313501537815D+00
                  w(8)=0.64751609634366950948233990524220090D+00
                  w(9)=0.71531936131009210730689383323862597D+00
                  w(10)=0.78056820684536627013137365914408900D+00
                  w(11)=0.84375176970158327910880470932055524D+00
                  w(12)=0.90534951694627313366320356420610166D+00
                  w(13)=0.96584102941677971641589491980492362D+00
                  w(14)=0.10257164140984820410234768329204344D+01
                  w(15)=0.10854770634509057335130907348058280D+01
                  w(16)=0.11456126256324485166998558090768066D+01
                  w(17)=0.12065385065152341387991189637581640D+01
                  w(18)=0.12684988767687596325545544799233829D+01
                  w(19)=0.13314885323953722347881897037112337D+01
                  w(20)=0.13952704199809699030269325768064586D+01
                  w(21)=0.14595114553199226162749562940524050D+01
                  w(22)=0.15239759042388420312516951668572480D+01
                  w(23)=0.15886957531833074295995312836430832D+01
                  w(24)=0.16540897506752873399784460115448908D+01
                  w(25)=0.17210679536459592692815000418877582D+01
                  w(26)=0.17912073892261758262059821285746319D+01
                  w(27)=0.18671534771624405130955045240917516D+01
                  w(28)=0.19536073951870566842193397262100507D+01
                  w(29)=0.20598855521732198890753706958043732D+01
                  w(30)=0.22072691881018378242959576701288560D+01
            end if
            if (n == 31) then
                  x(1)=0.26822925679680286121878407769380623D-01
                  x(2)=0.14082617983290539598175356628725276D+00
                  x(3)=0.34397374233514768423664964895054695D+00
                  x(4)=0.63331083122225728756771297177629119D+00
                  x(5)=0.10051430102732797992724709809793843D+01
                  x(6)=0.14555074753887195736708730895368740D+01
                  x(7)=0.19805142745213459308277445210395898D+01
                  x(8)=0.25765635576452984456951042844661122D+01
                  x(9)=0.32404589392390721644827466247087691D+01
                  x(10)=0.39694505268045488404586342642695802D+01
                  x(11)=0.47612398136316936435837317723214707D+01
                  x(12)=0.56139707742231875853793525817241946D+01
                  x(13)=0.65262224685942442660170696687067544D+01
                  x(14)=0.74970102164038925939450965312459849D+01
                  x(15)=0.85257948454057095586345290602855649D+01
                  x(16)=0.96124911778244792141654507288704395D+01
                  x(17)=0.10757455751924926688109338188796697D+02
                  x(18)=0.11961422218511974549693768825630772D+02
                  x(19)=0.13225356976726298295038363828927290D+02
                  x(20)=0.14550247303220059988272313374249645D+02
                  x(21)=0.15936894300951686977744854889193155D+02
                  x(22)=0.17385808091827841455154247528511579D+02
                  x(23)=0.18897263262383704999984227338876843D+02
                  x(24)=0.20471507373315734231351728740319340D+02
                  x(25)=0.22109082818791363402030409298831740D+02
                  x(26)=0.23811242418020231645139431054851287D+02
                  x(27)=0.25580499953473671436664646965473755D+02
                  x(28)=0.27421461021267978587420545247687517D+02
                  x(29)=0.29342298398141092707678387577199943D+02
                  x(30)=0.31357765583994549293172409858011577D+02
                  x(31)=0.33495343311872392849229444884494005D+02
                  w(1)=0.68778050331675868034281483898472378D-01
                  w(2)=0.15897683080860598653468000864483152D+00
                  w(3)=0.24681452952971766749634484349950867D+00
                  w(4)=0.33123621032513026973811715283977627D+00
                  w(5)=0.41176197004205766979552385830397147D+00
                  w(6)=0.48831507046741919572701061928747055D+00
                  w(7)=0.56109674940196189902555364747331512D+00
                  w(8)=0.63046844769223036964882447167642735D+00
                  w(9)=0.69686397669898038470538378319045804D+00
                  w(10)=0.76073629838618082069681748507898876D+00
                  w(11)=0.82253307739347180504005453548749278D+00
                  w(12)=0.88269199993448555910009495535675304D+00
                  w(13)=0.94164710152438560108473290713621664D+00
                  w(14)=0.99983834435696702670802575942004116D+00
                  w(15)=0.10577168416646722481046444710651631D+01
                  w(16)=0.11157362405005394843898759454305571D+01
                  w(17)=0.11743176054381855588298576223310114D+01
                  w(18)=0.12337801138273062935277145266128049D+01
                  w(19)=0.12942576640396468997285285427393965D+01
                  w(20)=0.13556584026982073104990915153704844D+01
                  w(21)=0.14177196548844306917320805419096441D+01
                  w(22)=0.14801500983913070792020252242512755D+01
                  w(23)=0.15427960790108570246389524107900434D+01
                  w(24)=0.16057757121101703085850937371594506D+01
                  w(25)=0.16695751758548637924858171761131298D+01
                  w(26)=0.17351502264802878727328808210002327D+01
                  w(27)=0.18041117536429376408251785780807404D+01
                  w(28)=0.18791398180079142418527247271043387D+01
                  w(29)=0.19649864575175979462695249953221048D+01
                  w(30)=0.20710824475632701040699417453694388D+01
                  w(31)=0.22190621157913884053467579686861871D+01
            end if
            if (n == 32) then
                  x(1)=0.25998362939364637114742457871867308D-01
                  x(2)=0.13652671374710292053882746709510325D+00
                  x(3)=0.33359291782842932105895551520669969D+00
                  x(4)=0.61448482002046767381409818940569002D+00
                  x(5)=0.97577889660595703343416039592784835D+00
                  x(6)=0.14137649040335828421208333275996209D+01
                  x(7)=0.19247609158370738759370322760992363D+01
                  x(8)=0.25053220245434789834144185673921628D+01
                  x(9)=0.31523558833038436459405106170364854D+01
                  x(10)=0.38631719176078571648907330539497646D+01
                  x(11)=0.46354918178097799495243651436465726D+01
                  x(12)=0.54674433202148092427265004527477984D+01
                  x(13)=0.63575520496082701158326726301821985D+01
                  x(14)=0.73047393656899555446108868563574609D+01
                  x(15)=0.83083280398156081219517213709629825D+01
                  x(16)=0.93680514279227815024631397516423681D+01
                  x(17)=0.10484054107842826970412025291553901D+02
                  x(18)=0.11656862190436381351030303374719145D+02
                  x(19)=0.12887296078129937173861322035239876D+02
                  x(20)=0.14176313661714869384823758954043702D+02
                  x(21)=0.15524815197243562941020814873021128D+02
                  x(22)=0.16933484660774438160982610181549940D+02
                  x(23)=0.18402743267903489859432861833027831D+02
                  x(24)=0.19932845375676646508662291686018114D+02
                  x(25)=0.21524097580227361087802531180606030D+02
                  x(26)=0.23177167911208819483622070804738057D+02
                  x(27)=0.24893479691573595921626830354180208D+02
                  x(28)=0.26675743736409775749265043544226918D+02
                  x(29)=0.28528782969290113121373638724100270D+02
                  x(30)=0.30461022188226196034933234344163400D+02
                  x(31)=0.32487560393070941790399777804430128D+02
                  x(32)=0.34636535667057931954771511821474253D+02
                  w(1)=0.66667239848937128539700718380869641D-01
                  w(2)=0.15416359064486569253003516678324319D+00
                  w(3)=0.23950658080619059166410142689636319D+00
                  w(4)=0.32169972520478185602416221178629160D+00
                  w(5)=0.40026477172670987703950143937152067D+00
                  w(6)=0.47509014836897866479218215883850104D+00
                  w(7)=0.54632586761632059316974332016073733D+00
                  w(8)=0.61428020649459143964896091059753151D+00
                  w(9)=0.67933895990546320537764813595141249D+00
                  w(10)=0.74191352897446409549409505496699677D+00
                  w(11)=0.80241438199484540689963799483790501D+00
                  w(12)=0.86124293381132904817428793117460479D+00
                  w(13)=0.91879464726183564448283753331430564D+00
                  w(14)=0.97546693320163668419585959633361365D+00
                  w(15)=0.10316659112778857589917093560428736D+01
                  w(16)=0.10878053827322333636973694561125679D+01
                  w(17)=0.11442890731278576601008983399734721D+01
                  w(18)=0.12014665114910733433872173404503975D+01
                  w(19)=0.12595643632905671506902128044174640D+01
                  w(20)=0.13186236937191379499859752321072310D+01
                  w(21)=0.13784942208040575136430640600704149D+01
                  w(22)=0.14389132475376569812840932276358458D+01
                  w(23)=0.14996426117598779383343854344996668D+01
                  w(24)=0.15606075279646514599829451851684251D+01
                  w(25)=0.16220017812466973587234900307883343D+01
                  w(26)=0.16843676170897712896402736973188686D+01
                  w(27)=0.17486950937056126482302406268640291D+01
                  w(28)=0.18166158695616910685235012183278741D+01
                  w(29)=0.18908330309204126945993202977662983D+01
                  w(30)=0.19761513512044362459487019437572481D+01
                  w(31)=0.20821624371414573786942215584442113D+01
                  w(32)=0.22311518737801328454419306397303769D+01
            end if
            if (n == 33) then
                  x(1)=0.25222297727341367045341498090288279D-01
                  x(2)=0.13247759128084673125693537169240197D+00
                  x(3)=0.32380669248023829406786727546666573D+00
                  x(4)=0.59671535823233034137651513973224040D+00
                  x(5)=0.94802704428024792678243421242021407D+00
                  x(6)=0.13742659894203065380488875159589271D+01
                  x(7)=0.18719475451288242476860654200118929D+01
                  x(8)=0.24377774897497587479818881464383961D+01
                  x(9)=0.30687681826830447541205013447626959D+01
                  x(10)=0.37622927481697931268943061562359328D+01
                  x(11)=0.45161007854489040086661125030171500D+01
                  x(12)=0.53283153101708054031225320864177082D+01
                  x(13)=0.61974248981676393816780680439064411D+01
                  x(14)=0.71222793332370720961149380454189601D+01
                  x(15)=0.81020919953769822353842156198052385D+01
                  x(16)=0.91364474341994357449936995208687141D+01
                  x(17)=0.10225307113296537513989347903991238D+02
                  x(18)=0.11368999161839710408370632245518171D+02
                  x(19)=0.12568170717361438067511960780294568D+02
                  x(20)=0.13823683086431786528545547245249126D+02
                  x(21)=0.15136452987314823214345607781331551D+02
                  x(22)=0.16507283044647312180023029174958931D+02
                  x(23)=0.17936750445208847246620767542379565D+02
                  x(24)=0.19425207754170004727028508505912445D+02
                  x(25)=0.20972906955216227074100247961112002D+02
                  x(26)=0.22580224099961825172852081783576161D+02
                  x(27)=0.24247959886492883069638444450056581D+02
                  x(28)=0.25977718733384467051219033602380474D+02
                  x(29)=0.27772422510974584873602695903887498D+02
                  x(30)=0.29637118447457174103505248225480900D+02
                  x(31)=0.31580471971364524634422030567800803D+02
                  x(32)=0.33617895925272791712837896131882939D+02
                  x(33)=0.35778138778825404110838466101261522D+02
                  w(1)=0.64680274571235486036457406448166375D-01
                  w(2)=0.14962720607997426757623144566345455D+00
                  w(3)=0.23260602282627721622826604968081634D+00
                  w(4)=0.31267567589062273494307033234158867D+00
                  w(5)=0.38936352755688919062467532551366305D+00
                  w(6)=0.46253028974011123162168938980735346D+00
                  w(7)=0.53228190405863130293409006771213252D+00
                  w(8)=0.59887920022585349358827126709088577D+00
                  w(9)=0.66266415869490858465978495785176115D+00
                  w(10)=0.72400998959978495266241888131780811D+00
                  w(11)=0.78329340463991180357255156324037103D+00
                  w(12)=0.84088382086631108307071602443631704D+00
                  w(13)=0.89714360915324310111417124423945526D+00
                  w(14)=0.95243401551568252408440862336998988D+00
                  w(15)=0.10071219382388538809845260440828009D+01
                  w(16)=0.10615826335963537757254172531082489D+01
                  w(17)=0.11161921501942585458724324606556395D+01
                  w(18)=0.11713015487646932230587234486315359D+01
                  w(19)=0.12271878679241545470036097952985308D+01
                  w(20)=0.12839926657455337719925109446216773D+01
                  w(21)=0.13416829044598758092107490543369267D+01
                  w(22)=0.14000734153500636663107041407510919D+01
                  w(23)=0.14589180106367171958215288768885336D+01
                  w(24)=0.15180347903942602331660913820838569D+01
                  w(25)=0.15774212769044863899733583225920428D+01
                  w(26)=0.16373402708097612689856542241330695D+01
                  w(27)=0.16983905310960810483780162816202082D+01
                  w(28)=0.17616017849285616755158600643228910D+01
                  w(29)=0.18286259025840428251806321932856955D+01
                  w(30)=0.19021749030833894875179072680083626D+01
                  w(31)=0.19870826536338645428119671460261962D+01
                  w(32)=0.20930613125138041816695221597783312D+01
                  w(33)=0.22428277080254511711796644138559824D+01
            end if
      end subroutine quad_ExpSum_Complex


      subroutine quad_DoubleExponential_Cos(x, w, omega, M, n)
            !
            ! Compute points and weights of the double exponential quadrature for
            ! the cosine integral:
            !
            ! Int(0,Inf) f(t) cos(omega t) dt = Sum(k=1,2*n+1) wk * f(xk)
            !
            ! The accuracy of the quadrature is controlled by M and n
            ! (defined as in Ref. 1).
            !
            ! The arrays x and w should be of size 2*n+1.
            !
            ! 1. Ooura, T. and Mori, M. J. Comp. and Appl. Math. 112, 229 (1999);
            !    doi: 10.1016/S0377-0427(99)00223-X
            !
            real(F64), dimension(:), intent(out) :: x
            real(F64), dimension(:), intent(out) :: w
            real(F64), intent(in)                :: omega
            integer, intent(in)                  :: M
            integer, intent(in)                  :: n

            real(F128) :: alpha, beta
            real(F128) :: h, t
            real(F128) :: phi, v, v_t, phi_t
            integer :: nk
            real(F128) :: An, Bn

            real(F128), parameter :: ONE = 1.0_F128
            real(F128), parameter :: TWO = 2.0_F128
            real(F128), parameter :: FOUR = 4.0_F128
            real(F128), parameter :: PI = FOUR * atan(ONE)
            !
            ! Eq. 2.6 in Ref. 1
            !
            h = PI / M
            !
            ! Eq. 3.6 in Ref. 1
            !
            beta = ONE / FOUR
            alpha = beta / sqrt(ONE + M * log(ONE + M) / (FOUR*PI))
            do nk = -n, n
                  !
                  ! Eq. 4.4 in Ref. 1
                  !
                  t = nk * h - PI / (TWO * M)
                  !
                  ! Eq. 3.3 in Ref. 1
                  ! Hyperbolic functions are used to reduce roundoff errors
                  !
                  v = -TWO * t - (alpha+beta) * sinh(t) + (alpha-beta) * cosh(t) + beta-alpha
                  v_t = -TWO - (alpha+beta) * cosh(t) + (alpha-beta) * sinh(t)
                  phi = -(ONE/TWO) * exp(-v/TWO) * t / sinh(v/TWO)
                  phi_t = -(ONE/TWO) * exp(-v/TWO) / sinh(v/TWO) * ( &
                        ONE - (ONE/TWO) * t * v_t - (ONE/TWO) * t / tanh(v/TWO) * v_t)
                  !
                  ! Eq. 4.2 in Ref. 1
                  !
                  An = M * phi
                  Bn = phi_t * cos(An)
                  x(nk+n+1) = real(An, F64) / omega
                  w(nk+n+1) = real(PI * Bn, F64) / omega
            end do
      end subroutine quad_DoubleExponential_Cos

      
      subroutine quad_ClenshawCurtis_Type2(x, w, n, alpha)
            !
            ! Compute points and weights of the double exponential quadrature for
            ! the cosine integral:
            !
            ! Int(0,Inf) f(t) dt = Sum(k=1,n) wk * f(xk)
            !
            ! The formula used for nodes and weights is the formula 2 of Ref. 2.
            ! The use of Clenshaw-Curties quadratures for RPA is described in detail
            ! by Eschuis et al. The scaling factor alpha is labeled L in Boyd's paper.
            !
            ! 1. Boyd, J.P. J. Sci. Comput. 2, 99 (1987); doi: 10.1007/BF01061480
            ! 2. Eschuis, H., Yarkony, J., and Furche, F., J. Chem. Phys. 132, 234114 (2010);
            !    doi: 10.1063/1.3442749
            !
            real(F64), dimension(:), intent(out) :: x
            real(F64), dimension(:), intent(out) :: w
            integer, intent(in)                  :: n
            real(F64), intent(in)                :: alpha

            integer :: i, j
            real(F64) :: ti, s

            do i = 1, n
                  ti = PI * i / real(n+1, F64)
                  x(i) = alpha * (cos(ti/TWO)/sin(ti/TWO))**2
                  s = ZERO
                  do j = 1, n
                        s = s + sin(j * ti) * (ONE - cos(j * PI)) / j
                  end do
                  w(i) = TWO * alpha * sin(ti) / (ONE - cos(ti))**2 * TWO / real(n+1, F64) * s
            end do
      end subroutine quad_ClenshawCurtis_Type2


      subroutine quad_AdiabaticConnection(x, w, n)
            !
            ! The roots and weights computed with this subroutine
            ! should be used as follows for an even n
            !
            ! int(0,1) f(x) dx = sum(k=1,n) w(k) * f(x(k)
            !
            ! For numerical stability, the Newton root finding is carried
            ! out in the theta variable (see Ref. 1). MaxAbsError corresponds
            ! to the errors in the theta variable.
            !
            ! 1. Townsend, A., Trogdon, T., and Olver, S. IMA J. Num. Analysis 36, 337 (2016);
            ! doi: 10.1093/imanum/drv002
            !
            !
            real(F64), dimension(:), intent(out) :: x
            real(F64), dimension(:), intent(out) :: w
            integer, intent(in)                  :: n

            logical :: converged
            integer :: k, k1, k2
            integer :: nGL
            real(F64), dimension(:), allocatable :: x_unsorted, w_unsorted
            integer, dimension(:), allocatable :: permutation
            real(F64), dimension(:), allocatable :: xGL, wGL
            real(F64), parameter :: MaxAbsError = 1.0E-14_F64

            nGL = n / 2 + modulo(n, 2)
            allocate(xGL(nGL))
            allocate(wGL(nGL))
            call quad_GaussLegendre(xGL, wGL, converged, n, MaxAbsError)
            if (.not. converged) then
                  call msg("Error while generating the Gauss-Legendre quadrature for adiabatic connection integral", MSG_ERROR)
                  error stop
            end if
            allocate(x_unsorted(n))
            allocate(w_unsorted(n))
            if (modulo(n,2) == 1) then
                  w_unsorted(1) = wGL(1) / TWO
                  x_unsorted(1) = ONE / TWO
                  do k = 2, n/2 + 1
                        k1 = 2*(k - 1)
                        k2 = 2*(k - 1) + 1
                        w_unsorted(k1) = wGL(k) / TWO
                        w_unsorted(k2) = wGL(k) / TWO
                        x_unsorted(k1) = (ONE-xGL(k)) / TWO
                        x_unsorted(k2) = (ONE+xGL(k)) / TWO
                  end do
            else
                  do k = 1, n/2
                        k1 = 1 + 2*(k - 1)
                        k2 = 2 + 2*(k - 1)
                        w_unsorted(k1) = wGL(k) / TWO
                        w_unsorted(k2) = wGL(k) / TWO
                        x_unsorted(k1) = (ONE-xGL(k)) / TWO
                        x_unsorted(k2) = (ONE+xGL(k)) / TWO
                  end do
            end if
            allocate(permutation(n))
            do k = 1, n
                  permutation(k) = k
            end do
            call dsort(x_unsorted, permutation, n)
            do k = 1, n
                  x(k) = x_unsorted(k)
                  w(k) = w_unsorted(permutation(k))
            end do
      end subroutine quad_AdiabaticConnection
end module quadratures
