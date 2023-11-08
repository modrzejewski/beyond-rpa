module cptxhole
      use math_constants
      use arithmetic
      use chebinterp
      
      implicit none
      save
      !
      ! Chebyshev interpolation of exp(x) Ei(-x)
      ! = -exp(x) * E1(x). The following set of parameters
      ! guarantees relative errors below 5.0E-15 for x > 0.
      !
      real(F64), parameter, private :: E1_X_SMALL = 1.0_F64
      real(F64), parameter, private :: E1_X_LARGE = 14.0_F64
      integer, parameter, private :: E1_NINTERVALS = 150
      integer, parameter, private :: E1_INTERP_N = 9
      real(F64), parameter, private :: E1_DELTAX = (E1_X_LARGE - E1_X_SMALL) / real(E1_NINTERVALS, F64)
      real(F64), dimension(:, :), allocatable, private :: E1_INTERPTABLE

contains

      subroutine cptxhole_init()
            integer :: i, j
            real(F64) :: a, b
            real(F64), dimension(E1_INTERP_N) :: e1_interp_x, e1_interp_f

            allocate(E1_INTERPTABLE(E1_INTERP_N, E1_NINTERVALS))
            !
            ! Generate coefficients of the expansion of exp(x) Ei(-x) in terms
            ! of the Chebyshev polynomials
            !
            do i = 1, E1_NINTERVALS
                  a = E1_X_SMALL + real(i - 1, F64) * E1_DELTAX
                  b = E1_X_SMALL + real(i, F64) * E1_DELTAX
                  call cheb_nodes(e1_interp_x, E1_INTERP_N, a, b)
                  do j = 1, E1_INTERP_N
                        e1_interp_f(j) = e1_exact(e1_interp_x(j))
                  end do
                  call cheb_coeffs(E1_INTERPTABLE(:, i), e1_interp_f, E1_INTERP_N)
            end do
      end subroutine cptxhole_init


      subroutine cptxhole_free()
            deallocate(E1_INTERPTABLE)
      end subroutine cptxhole_free


      pure function e1_interp(x)
            !
            ! Compute exp(x) * Ei(-x) = -exp(x) * E1(x) with relative
            ! errors below 5.0E-15
            !
            real(F64) :: e1_interp
            real(F64), intent(in)  :: x
            
            real(F64) :: ab_sum
            integer :: k

            if (x < E1_X_SMALL) then
                  e1_interp = e1_expansion(x, 16)
            else if (x < E1_X_LARGE) then
                  k = floor((x - E1_X_SMALL) / E1_DELTAX)
                  !
                  ! a = E1_X_SMALL + real(k, F64) * DELTAX
                  ! b = E1_X_SMALL + real(k+1, F64) * DELTAX
                  !
                  ab_sum = TWO*E1_X_SMALL + real(2 * k + 1, F64) * E1_DELTAX
                  call cheb_approx_9(e1_interp, x, E1_INTERPTABLE(:, k+1), ab_sum, E1_DELTAX)
            else
                  e1_interp = e1_cont_frac(x, 10)
            end if
      end function e1_interp


      pure function e1_exact(x)
            !
            ! Compute exp(x) * Ei(-x) = -exp(x) * E1(x) with accurate but slow method
            !
            real(F64) :: e1_exact
            real(F64), intent(in) :: x

            if (x < 1.0_F64) then
                  e1_exact = e1_expansion(x, 25)
            else
                  e1_exact = e1_cont_frac(x, 100)
            end if
      end function e1_exact


      pure function e1_cont_frac(x, n)
            !
            ! Compute exp(x) * Ei(-x) = -exp(x) * E1(x) using the large x continued 
            ! fraction expansion
            !
            real(F64) :: e1_cont_frac
            real(F64), intent(in) :: x
            integer, intent(in)   :: n

            real(F64) :: tk, rk
            integer :: k

            tk = ZERO
            do k = n, 1, -1
                  rk = real(k, F64)
                  tk = rk / (ONE + rk / (x + tk))
            end do
            e1_cont_frac = -ONE / (x + tk)
      end function e1_cont_frac


      pure function e1_expansion(x, n)
            !
            ! Compute exp(x) * Ei(-x) = -exp(x) * E1(x) using the small x expansion
            !
            real(F64) :: e1_expansion
            real(F64), intent(in) :: x
            integer, intent(in)   :: n

            real(F64) :: tk, rk, rk1, s
            integer :: k

            tk = ONE
            rk1 = ONE
            do k = 1, n
                  rk = real(k, F64)
                  tk = tk * (-x) * rk1 / rk**2
                  rk1 = rk
                  s = s + tk
            end do
            e1_expansion = exp(x) * (log(x) + EULER_CONSTANT + s)
      end function e1_expansion


      subroutine test_parameters(F, G, H, K, Fx, d1, d2, d3, d4, aA7)
            real(F64), intent(in) :: F, G, H, K, Fx, d1, d2, d3, d4, aA7

            real(F64), parameter :: E = -0.051955731_F64

            print *, "-------------TESTING EQ. A5 (ENERGY SUM RULE) ---------"
            print *, "LHS=", -NINE/EIGHT*Fx
            print *, "RHS=", d2 + E * G / d1**3 + d3 + THREE*K/d1**4
            print *, "------------ TESTING EQ. A6 (XHOLE SUM RULE) ----------"
            print *, "LHS=", E*G*FIFTEEN*sqrt(PI)/(SIXTEEN*d1**(SEVEN/TWO)) &
                  + aA7  + K*d4
            print *, "RHS=", -THREE*PI/FOUR
            print *, "-------------------------------------------------------"
      end subroutine test_parameters


      subroutine jcptxhole(Jc, y, Fx, s, z)
            real(F64), dimension(:), intent(out) :: Jc
            real(F64), dimension(:), intent(in)  :: y
            real(F64), intent(in)                :: Fx
            real(F64), intent(in)                :: s
            real(F64), intent(in)                :: z
            
            real(F64) :: F, F_s, F_z, G, G_Fx, G_s, G_z
            real(F64) :: H, H_s, H_z, K, K_Fx, K_s, K_z
            integer :: n, i
            real(F64) :: yi
            real(F64), parameter :: A = 1.0161144_F64
            real(F64), parameter :: B = -0.37170836_F64
            real(F64), parameter :: C = -0.077215461_F64
            real(F64), parameter :: D = 0.57786348_F64
            real(F64), parameter :: E = -0.051955731_F64

            real(F64) :: y2, y4, y6
            real(F64) :: t1, t2

            call jcptxhole_FGHK(F, F_s, F_z, G, G_Fx, G_s, G_z, H, H_s, H_z, K, K_Fx, K_s, K_z, Fx, s, z)

            n = size(y)
            do i = 1, n
                  yi = y(i)
                  y2 = yi**2
                  y4 = y2 * y2
                  y6 = y2 * y4
                  
                  t1 = -A/y2 * (ONE / (ONE + (FOUR/NINE)*A * y2)) * exp(-s**2 * H * y2)
                  t2 = (A/y2 + B + C * (ONE + F) * y2 + E * (ONE + G) * y4 + K * y6) * exp(-(D+s**2*H) * y2)


                  Jc(i) = t1 + t2 
                  !(-A/(y2 * (ONE + FOUR/NINE*A * yi**2)) + &
                        !(A/yi**2 + B + C * (ONE + F) * yi**2 + E * (ONE + G) * yi**4 + K * yi**6) * exp(-D*yi**2))&
                        !* exp(-s**2*H*yi**2)
            end do
      end subroutine jcptxhole


      subroutine jcptxhole_FGHK(F, F_s, F_z, G, G_Fx, G_s, G_z, H, H_s, H_z, K, K_Fx, K_s, K_z, Fx, s, z)
            real(F64), intent(out) :: F
            real(F64), intent(out) :: F_s
            real(F64), intent(out) :: F_z
            real(F64), intent(out) :: G
            real(F64), intent(out) :: G_Fx
            real(F64), intent(out) :: G_s
            real(F64), intent(out) :: G_z
            real(F64), intent(out) :: H
            real(F64), intent(out) :: H_s
            real(F64), intent(out) :: H_z
            real(F64), intent(out) :: K
            real(F64), intent(out) :: K_Fx
            real(F64), intent(out) :: K_s
            real(F64), intent(out) :: K_z
            real(F64), intent(in)  :: Fx
            real(F64), intent(in)  :: s
            real(F64), intent(in)  :: z

            real(F64) :: s2, s3, s4, s5, s6
            real(F64) :: d1, d1_s, d1_z, d2, d2_s, d2_z
            real(F64) :: d3, d3_s, d3_z, d4, d4_s, d4_z
            real(F64) :: aA7, aA7_s, aA7_z
            
            print *, "COMPUTING PARAMETERS OF XHOLE OF CONSTANTIN ET AL."
            print *, "S  = ", s
            print *, "Z  = ", z
            print *, "Fx = ", Fx

            s2 = s**2
            s3 = s2 * s
            s4 = s2 * s2
            s5 = s4 * s
            s6 = s3 * s3
            call Heq32(H, H_s, h_z, s, s2, s3, s4, s5, s6, z)
            print *, "H  = ", H
            
            call FeqA10(F, F_s, F_z, s, s2, s5, s6, z, H, H_s, H_z)
            print *, "F  = ", F

            call d1eqA1(d1, d1_s, d1_z, s, s2, H, H_s, H_z)
            print *, "d1 = ", d1

            call d2eqA2(d2, d2_s, d2_z, s, s2, H, H_s, H_z, F, F_s, F_z, d1, d1_s, d1_z)
            print *, "d2 = ", d2

            call d3eqA3(d3, d3_s, d3_z, s, s2, H, H_s, H_z, d1, d1_s, d1_z)
            print *, "d3 = ", d3

            call d4eqA4(d4, d4_s, d4_z, d1, d1_s, d1_z)
            print *, "d4 = ", d4

            call aeqA7(aA7, aA7_s, aA7_z, s, s2, H, H_s, H_z, F, F_s, F_z, d1, d1_s, d1_z)
            print *, "a  = ", aA7

            call KeqA11(K, K_s, K_z, K_Fx, Fx, aA7, aA7_s, aA7_z, d1, d1_s, d1_z, d2, d2_s, d2_z, d3, d3_s, d3_z)
            print *, "K  = ", K


            call GeqA12(G, G_Fx, G_s, G_z, K, K_Fx, K_s, K_z, d1, d1_s, d1_z, d4, d4_s, d4_z, aA7, aA7_s, aA7_z)
            print *, "G  = ", G


            call test_parameters(F, G, H, K, Fx, d1, d2, d3, d4, aA7)
      end subroutine jcptxhole_FGHK
      

      pure subroutine Heq32(h, h_s, h_z, s, s2, s3, s4, s5, s6, z)
            real(F64), intent(out) :: h
            real(F64), intent(out) :: h_s
            real(F64), intent(out) :: h_z
            real(F64), intent(in)  :: s
            real(F64), intent(in)  :: s2
            real(F64), intent(in)  :: s3
            real(F64), intent(in)  :: s4
            real(F64), intent(in)  :: s5
            real(F64), intent(in)  :: s6
            real(F64), intent(in)  :: z

            real(F64) :: z4, z5
            real(F64) :: x, y, x_s, y_s
            real(F64), parameter :: a1 = 0.122499_F64
            real(F64), parameter :: a2 = 0.121785_F64
            real(F64), parameter :: a3 = 0.066065_F64
            real(F64), parameter :: a4 = 0.187440_F64
            real(F64), parameter :: a5 = 0.00120824_F64
            real(F64), parameter :: a6 = 0.0347188_F64

            x = a1 + a2 * s2 + a3 * s4
            x_s = TWO*a2 * s + FOUR*a3 * s3
            y = ONE + a4 * s4 + a5 * s5 + a6 * s6
            y_s = FOUR*a4 * s3 + FIVE*a5 * s4 + SIX*a6 * s5
            z4 = z**4
            z5 = z4 * z
            h = z5 * x / y
            h_z = FIVE * z4 * x / y
            h_s = z5 * x_s / y - h / y * y_s
      end subroutine Heq32


      pure subroutine FeqA10(f, f_s, f_z, s, s2, s5, s6, z, h, h_s, h_z)
            real(F64), intent(out) :: f
            real(F64), intent(out) :: f_s
            real(F64), intent(out) :: f_z
            real(F64), intent(in)  :: s            
            real(F64), intent(in)  :: s2
            real(F64), intent(in)  :: s5
            real(F64), intent(in)  :: s6
            real(F64), intent(in)  :: z
            real(F64), intent(in)  :: h
            real(F64), intent(in)  :: h_s
            real(F64), intent(in)  :: h_z

            real(F64), parameter :: C = -0.077215461_F64
            !
            ! c parameter of Eq. 33
            !
            real(F64), parameter :: cl = 0.00012_F64
            real(F64) :: L, L_s
            real(F64) :: t, t13
            real(F64) :: w, w_z

            t = ONE + cl * s6
            t13 = t**(ONE/THREE)
            L = ONE/THREE * s2 / t13
            L_s = ONE/THREE * TWO * s / t13 - (ONE/THREE) * L / t * (SIX*cl * s5)
            w = ONE/z + ONE
            w_z = -ONE/z**2
            F = ONE/(TWO*C) * (L * w - ONE/FIVE - s2 * H)
            F_s = ONE/(TWO*C) * (L_s * w - TWO * s * H - s2 * H_s)
            F_z = ONE/(TWO*C) * (L * w_z - s2 * H_z)
      end subroutine FeqA10


      pure subroutine d1eqA1(d1, d1_s, d1_z, s, s2, H, H_s, H_z)
            real(F64), intent(out) :: d1
            real(F64), intent(out) :: d1_s
            real(F64), intent(out) :: d1_z
            real(F64), intent(in)  :: s
            real(F64), intent(in)  :: s2
            real(F64), intent(in)  :: h
            real(F64), intent(in)  :: h_s
            real(F64), intent(in)  :: h_z
            
            real(F64), parameter :: D = 0.57786348_F64

            d1 = D + H * s2
            d1_s = H_s * s2 + TWO * H * s
            d1_z = H_z * s2
      end subroutine d1eqA1


      pure subroutine d2eqA2(d2, d2_s, d2_z, s, s2, h, h_s, h_z, f, f_s, f_z, d1, d1_s, d1_z)
            real(F64), intent(out) :: d2
            real(F64), intent(out) :: d2_s
            real(F64), intent(out) :: d2_z
            real(F64), intent(in)  :: s
            real(F64), intent(in)  :: s2
            real(F64), intent(in)  :: h
            real(F64), intent(in)  :: h_s
            real(F64), intent(in)  :: h_z
            real(F64), intent(in)  :: f
            real(F64), intent(in)  :: f_s
            real(F64), intent(in)  :: f_z
            real(F64), intent(in)  :: d1
            real(F64), intent(in)  :: d1_s
            real(F64), intent(in)  :: d1_z

            real(F64), parameter :: B = -0.37170836_F64
            real(F64), parameter :: C = -0.077215461_F64
            real(F64), parameter :: D = 0.57786348_F64
            real(F64), parameter :: E = -0.051955731_F64
            real(F64) :: t, t_s, t_z
            real(F64) :: w0, w1, w2, w3, w4, w5, w6
            real(F64) :: w1_s, w2_s, w3_s, w4_s, w5_s, w6_s
            real(F64) :: w1_z, w2_z, w3_z, w4_z, w5_z, w6_z

            t = H * s2
            t_s = H_s * s2 + TWO * H * s
            t_z = H_z * s2

            w0 = C*D + B*D**2 + TWO*E

            w1 = C*D * F
            w1_s = C*D * F_s
            w1_z = C*D * F_z

            w2 = (C+TWO*B*D) * t
            w2_s = (C+TWO*B*D) * t_s
            w2_z = (C+TWO*B*D) * t_z

            w3 = C * F * t
            w3_s = C * (F_s * t + F * t_s)
            w3_z = C * (F_z * t + F * t_z)

            w4 = B * t**2
            w4_s = TWO*B * t * t_s
            w4_z = TWO*B * t * t_z

            w5 = ONE/(TWO*d1**3)
            w5_s = -THREE * w5 / d1 * d1_s
            w5_z = -THREE * w5 / d1 * d1_z

            w6 = w0 + w1 + w2 + w3 + w4
            w6_s = w1_s + w2_s + w3_s + w4_s
            w6_z = w1_z + w2_z + w3_z + w4_z

            d2 = w6 * w5
            d2_s = w6_s * w5 + w6 * w5_s
            d2_z = w6_z * w5 + w6 * w5_z
      end subroutine d2eqA2


      pure subroutine d3eqA3(d3, d3_s, d3_z, s, s2, h, h_s, h_z, d1, d1_s, d1_z)
            real(F64), intent(out) :: d3
            real(F64), intent(out) :: d3_s
            real(F64), intent(out) :: d3_z
            real(F64), intent(in)  :: s
            real(F64), intent(in)  :: s2
            real(F64), intent(in)  :: h
            real(F64), intent(in)  :: h_s
            real(F64), intent(in)  :: h_z
            real(F64), intent(in)  :: d1
            real(F64), intent(in)  :: d1_s
            real(F64), intent(in)  :: d1_z
            
            real(F64), parameter :: A = 1.0161144_F64
            real(F64) :: x, x_s, x_z, y, y_s, y_z, t, t_s, t_z
            real(F64) :: exp_ei, exp_ei_s, exp_ei_z

            t = H * s2
            t_s = H_s * s2 + TWO * H * s
            t_z = H_z * s2
            x = t / d1
            x_s = t_s / d1 - x / d1 * d1_s
            x_z = t_z / d1 - x / d1 * d1_z
            y = NINE/(FOUR*A) * t
            y_s = NINE/(FOUR*A) * t_s
            y_z = NINE/(FOUR*A) * t_z
            exp_ei = e1_interp(y)
            exp_ei_s = (exp_ei + ONE/y) * y_s
            exp_ei_z = (exp_ei + ONE/y) * y_z            
            d3 = (A/TWO) * (log(x) - exp_ei)
            d3_s = (A/TWO) * (x_s/x - exp_ei_s)
            d3_z = (A/TWO) * (x_z/x - exp_ei_z)
      end subroutine d3eqA3


      pure subroutine d4eqA4(d4, d4_s, d4_z, d1, d1_s, d1_z)
            real(F64), intent(out) :: d4
            real(F64), intent(out) :: d4_s
            real(F64), intent(out) :: d4_z
            real(F64), intent(in)  :: d1
            real(F64), intent(in)  :: d1_s
            real(F64), intent(in)  :: d1_z

            d4 = (105.0_F64/32.0_F64)*sqrt(PI) / (d1**4 * sqrt(d1))
            d4_s = -NINE/TWO * d4 / d1 * d1_s
            d4_z = -NINE/TWO * d4 / d1 * d1_z
      end subroutine d4eqA4


      pure subroutine erfc_x_exp_x2(x, v, v_x)
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
      end subroutine erfc_x_exp_x2

      
      pure subroutine aeqA7(aA7, aA7_s, aA7_z, s, s2, h, h_s, h_z, f, f_s, f_z, d1, d1_s, d1_z)
            real(F64), intent(out) :: aA7
            real(F64), intent(out) :: aA7_s
            real(F64), intent(out) :: aA7_z
            real(F64), intent(in)  :: s
            real(F64), intent(in)  :: s2
            real(F64), intent(in)  :: h
            real(F64), intent(in)  :: h_s
            real(F64), intent(in)  :: h_z
            real(F64), intent(in)  :: f
            real(F64), intent(in)  :: f_s
            real(F64), intent(in)  :: f_z
            real(F64), intent(in)  :: d1
            real(F64), intent(in)  :: d1_s
            real(F64), intent(in)  :: d1_z
            
            real(F64), parameter :: A = 1.0161144_F64
            real(F64), parameter :: B = -0.37170836_F64
            real(F64), parameter :: C = -0.077215461_F64
            real(F64), parameter :: E = -0.051955731_F64

            real(F64) :: x, x_s, x_z
            real(F64) :: v, v_x, v_s, v_z
            real(F64) :: w, w_s, w_z
            real(F64) :: t, t_s, t_z
            real(F64) :: r1, r1_s, r1_z
            real(F64) :: r2, r2_s, r2_z
            real(F64) :: r3, r3_s, r3_z
            real(F64) :: H12

            H12 = sqrt(H)
            x = THREE/(TWO*sqrt(A)) * s * H12
            x_s = THREE/(TWO*sqrt(A)) * (H12 + (ONE/TWO) * s * H_s / H12)
            x_z = THREE/(TWO*sqrt(A)) * (ONE/TWO) * s * H_z / H12
            call erfc_x_exp_x2(x, v, v_x)
            w = THREE*PI*sqrt(A)/FOUR * v
            w_s = THREE*PI*sqrt(A)/FOUR * v_x * x_s
            w_z = THREE*PI*sqrt(A)/FOUR * v_x * x_z
            t = sqrt(PI)/(SIXTEEN * d1**3 * sqrt(d1))
            t_s = -(SEVEN/TWO) * t / d1 * d1_s
            t_z = -(SEVEN/TWO) * t / d1 * d1_z
            r1 = FIFTEEN*E + SIX*C * (ONE + F) * d1
            r1_s = SIX*C * (F_s * d1 + (ONE + F) * d1_s)
            r1_z = SIX*C * (F_z * d1 + (ONE + F) * d1_z)
            r2 = FOUR*B * d1**2
            r2_s = EIGHT*B * d1 * d1_s
            r2_z = EIGHT*B * d1 * d1_z
            r3 = EIGHT*A * d1**3
            r3_s = THREE*EIGHT*A * d1**2 * d1_s
            r3_z = THREE*EIGHT*A * d1**2 * d1_z
            aA7 = (r1 + r2 + r3) * t - w
            aA7_s = (r1_s + r2_s + r3_s) * t + (r1 + r2 + r3) * t_s - w_s
            aA7_z = (r1_z + r2_z + r3_z) * t + (r1 + r2 + r3) * t_z - w_z
      end subroutine aeqA7


      pure subroutine KeqA11(K, K_s, K_z, K_Fx, Fx, aA7, aA7_s, aA7_z, &
            d1, d1_s, d1_z, d2, d2_s, d2_z, d3, d3_s, d3_z)
            real(F64), intent(out) :: K
            real(F64), intent(out) :: K_s
            real(F64), intent(out) :: K_z
            real(F64), intent(out) :: K_Fx
            real(F64), intent(in)  :: Fx
            real(F64), intent(in)  :: aA7
            real(F64), intent(in)  :: aA7_s
            real(F64), intent(in)  :: aA7_z
            real(F64), intent(in)  :: d1
            real(F64), intent(in)  :: d1_s
            real(F64), intent(in)  :: d1_z
            real(F64), intent(in)  :: d2
            real(F64), intent(in)  :: d2_s
            real(F64), intent(in)  :: d2_z
            real(F64), intent(in)  :: d3
            real(F64), intent(in)  :: d3_s
            real(F64), intent(in)  :: d3_z
            
            real(F64) :: t, t_s, t_z
            real(F64) :: w, w_s, w_z, d1_12
            real(F64) :: r, r_Fx, r_s, r_z

            t = -TWO * d1**4
            t_s = -EIGHT * d1**3 * d1_s
            t_z = -EIGHT * d1**3 * d1_z
            d1_12 = sqrt(d1)
            w = (THREE*PI/FOUR + aA7) * SIXTEEN/(FIFTEEN*sqrt(PI)) * d1_12
            w_s = aA7_s * SIXTEEN/(FIFTEEN*sqrt(PI)) * d1_12 + ONE/TWO * w / d1 * d1_s
            w_z = aA7_z * SIXTEEN/(FIFTEEN*sqrt(PI)) * d1_12 + ONE/TWO * w / d1 * d1_z
            r = -NINE/EIGHT * Fx - d2 + w - d3
            r_Fx = -NINE/EIGHT
            r_s = -d2_s + w_s - d3_s
            r_z = -d2_z + w_z - d3_z
            K = t * r
            K_Fx = t * r_Fx
            K_s = t_s * r + t * r_s
            K_z = t_z * r + t * r_z
      end subroutine KeqA11


      pure subroutine GeqA12(G, G_Fx, G_s, G_z, K, K_Fx, K_s, K_z, d1, d1_s, d1_z, d4, d4_s, d4_z, aA7, aA7_s, aA7_z)
            real(F64), intent(out) :: G
            real(F64), intent(out) :: G_Fx
            real(F64), intent(out) :: G_s
            real(F64), intent(out) :: G_z
            real(F64), intent(in)  :: K
            real(F64), intent(in)  :: K_Fx
            real(F64), intent(in)  :: K_s
            real(F64), intent(in)  :: K_z
            real(F64), intent(in)  :: d1
            real(F64), intent(in)  :: d1_s
            real(F64), intent(in)  :: d1_z
            real(F64), intent(in)  :: d4
            real(F64), intent(in)  :: d4_s
            real(F64), intent(in)  :: d4_z
            real(F64), intent(in)  :: aA7
            real(F64), intent(in)  :: aA7_s
            real(F64), intent(in)  :: aA7_z

            real(F64), parameter :: E = -0.051955731_F64
            real(F64) :: t, t_s, t_z
            real(F64) :: w, w_Fx, w_s, w_z

            t = ONE/E * SIXTEEN/(FIFTEEN*sqrt(PI)) * d1**3 * sqrt(d1)
            t_s = SEVEN/TWO * t / d1 * d1_s
            t_z = SEVEN/TWO * t / d1 * d1_z
            w = -THREE*PI/FOUR - aA7 - K * d4
            w_Fx = -K_Fx * d4
            w_s = -aA7_s - K_s * d4 - K * d4_s
            w_z = -aA7_z - K_z * d4 - K * d4_z
            G = t * w
            G_Fx = t * w_Fx
            G_s = t_s * w + t * w_s
            G_z = t_z * w + t * w_z
      end subroutine GeqA12


SUBROUTINE COMPE1(E1, X)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      REAL(F64) E1, X
      INTEGER :: M, K
        IF (X.EQ.0.0) THEN
           E1=1.0D+300
        ELSE IF (X.LE.1.0) THEN
           E1=1.0D0
           R=1.0D0
           DO 10 K=1,25
              R=-R*K*X/(K+1.0D0)**2
              E1=E1+R
              IF (DABS(R).LE.DABS(E1)*1.0D-15) GO TO 15
10         CONTINUE
15         GA=0.57721566490153280D+0
           E1=-GA-DLOG(X)+X*E1
        ELSE
           M=20+INT(80.0/X)
           T0=0.0D0
           DO 20 K=M,1,-1
              T0=K/(1.0D0+K/(X+T0))
20         CONTINUE
           T=1.0D0/(X+T0)
           E1=DEXP(-X)*T
        ENDIF
        RETURN
        END SUBROUTINE
end module cptxhole
