module orbextension
      use math_constants
      use arithmetic
      use spherh
      use gparam
      use display

      implicit none

      integer, parameter :: ORBEXT_RADIUS = 1
      integer, parameter :: ORBEXT_SQRADIUS = 2

contains

      function gamma_half(m)
            !
            ! Compute Gamma(M/2), where M is an integer number.
            ! Two cases are considered, for M being even or odd.
            ! GAMMA_HALF is defined for M >= 1.
            !
            real(F64) :: gamma_half
            integer, intent(in) :: m
            
            integer :: n
            
            n = m / 2
            if (modulo(m, 2) == 1) then
                  !
                  ! Gamma(n+1/2) = (2n-1)!! * Sqrt(Pi) / 2^n
                  !
                  gamma_half = dblfact(2 * n - 1) * pi12 / TWO**n
            else
                  gamma_half = fact(n - 1)
            end if
      end function gamma_half

      
      function int_rm_exp(alpha, m)
            !
            ! Definite integral,
            ! I = \int_0^\infty r^m \exp(-\alpha r^2) d r = 
            ! = \frac{1}{2 * \sqrt{\alpha}^{m+1}} \Gamma(\frac{m+1}{2})
            !
            ! I is defined for M >= 0 and ALPHA > 0.
            !
            real(F64) :: int_rm_exp
            real(F64), intent(in) :: alpha
            integer, intent(in)   :: m
            
            real(F64) :: a, b

            a = FRAC12 / sqrt(alpha)**(m+1)
            b = gamma_half(m + 1)
            int_rm_exp = b * a
      end function int_rm_exp

      
      function orbext_r1(l, np, cntr, expn)
            ! ------------------------------------------------------
            ! Compute average radius of a Gaussian orbital,
            ! \frac{\int r \phi_k dx dy dz}{\int \phi_k dx dy dz},
            ! where \phi_k is a contracted Gaussian function.
            ! ------------------------------------------------------
            ! L
            !     Cartesian angular momentum
            ! NP
            !     Number of primitive Gaussian functions
            ! CNTR
            !     Contraction coefficients 
            ! EXPN
            !     Exponents
            !
            real(F64)                           :: orbext_r1
            integer, intent(in)                 :: l
            integer, intent(in)                 :: np
            real(F64), dimension(:), intent(in) :: cntr
            real(F64), dimension(:), intent(in) :: expn
            

            integer :: k
            real(F64) :: a, b, c, alpha
            
            a = ZERO
            b = ZERO
            do k = 1, np
                  c = cntr(k)
                  alpha = expn(k)
                  !
                  ! Note the Jacobian (r^2)
                  !
                  a = a + c * int_rm_exp(alpha, l + 3)
                  b = b + c * int_rm_exp(alpha, l + 2)
            end do

            orbext_r1 = a / b
      end function orbext_r1


      function orbext_r2(l, np, cntr, expn)
            ! ------------------------------------------------------
            ! Compute average radius of a Gaussian orbital,
            ! \frac{\int r \phi_k dx dy dz}{\int \phi_k dx dy dz},
            ! where \phi_k is a contracted Gaussian function.
            ! ------------------------------------------------------
            ! L
            !     Cartesian angular momentum
            ! NP
            !     Number of primitive Gaussian functions
            ! CNTR
            !     Contraction coefficients 
            ! EXPN
            !     Exponents
            !
            real(F64)                           :: orbext_r2
            integer, intent(in)                 :: l
            integer, intent(in)                 :: np
            real(F64), dimension(:), intent(in) :: cntr
            real(F64), dimension(:), intent(in) :: expn
            

            integer :: k
            real(F64) :: a, b, c, alpha
            
            a = ZERO
            b = ZERO
            do k = 1, np
                  c = cntr(k)
                  alpha = expn(k)
                  !
                  ! Note the Jacobian (r^2)
                  !
                  a = a + c * int_rm_exp(alpha, l + 4)
                  b = b + c * int_rm_exp(alpha, l + 2)
            end do

            orbext_r2 = sqrt(a / b)
      end function orbext_r2

      subroutine orbext(a, idef)
            !
            ! Calculate extensions of all orbital shells
            ! in the basis set. The orbital extension 
            ! is defined as:
            !
            ! 1) Average radius if IDEF == ORBEXT_RADIUS,
            ! 2) Root-average-squared radius if IFDEF == ORBEXT_SQRADIUS.
            !
            ! The array A should be of dimension N >= NSHELL.
            ! On exit, A(K) is the extension of the K-th shell.
            ! All orbitals belonging to the same shell have the 
            ! same extension.
            !
            real(F64), dimension(:), intent(out) :: a
            integer, intent(in)                  :: idef

            integer :: s, shell_class, np, l

            if (idef == ORBEXT_RADIUS) then
                  do s = 1, NSHELL
                        shell_class = SH(s)
                        np = NPRM(shell_class)
                        l = shtype(shell_class)
                        a(s) = orbext_r1(l, np, CNTR(:, shell_class), EXPN(:, shell_class))
                  end do
            else if (idef == ORBEXT_SQRADIUS) then
                  do s = 1, NSHELL
                        shell_class = SH(s)
                        np = NPRM(shell_class)
                        l = shtype(shell_class)
                        a(s) = orbext_r2(l, np, CNTR(:, shell_class), EXPN(:, shell_class))
                  end do
            else
                  call msg("INVALID DEFINITION OF ORBITAL RADIUS", &
                        MSG_ERROR)
                  stop
            end if
      end subroutine orbext
end module orbextension
