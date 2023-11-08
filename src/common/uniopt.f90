!--------------------------------------------------------------------------------
! MINIMIZATION OF A UNIMODAL SINGLE-VARIABLE FUNCTION BY BRENT'S METHOD
! (COMBINED QUADRATIC INTERPOLATION AND GOLDEN SECTION)
! -------------------------------------------------------------------------------
! This is an implementation of Brent's algorithm for minimization of unimodular
! functions of single variable. This algorithm is useful when derivatives of
! the objective function are not available. A slight modification is made in
! order to improve the worst-case convergence of the original algorithm
! (see comments in TIGHTENBRACKET subroutine for details.)
!
! User should provide initial three points which need not bracket the 
! minimum of the objective function. (The convergence is faster if these three
! points do bracket the minimum.) Maximal and minimal allowed abscissa should
! also be provided. This algorithm is able to converge to a minimum lying on
! the boundary. If a triplet of points X1 < X2 < X3 is encountered for which
! F(X1) < F(X2) .and. F(X2) > F(X3), the algorithm will return
! UNI_MULTIPLE_MINIMA error code.
!
! This code is thread-safe.
! --------------------------------------------------------------------------------
! 1. Brent, R.P., An algorithm with guaranteed convergence for finding a minimum
!    of a function of one variable [in] Algorithms for minimization without
!    derivatives, Dover Publications, 2002, chapter 5.
!
module uniopt
      use display
      use arithmetic
      use math_constants
      use sort

      implicit none

      type tunidata
            real(F64), dimension(3) :: x
            real(F64), dimension(3) :: f
            real(F64) :: abstol
            integer :: max_niter
            real(F64) :: next_abscissa
            real(F64) :: min_abscissa
            real(F64) :: max_abscissa
            integer :: niter
            logical :: isbracketed
            logical :: lquadratic
            integer :: ipredicted
            integer :: iactual
      end type tunidata
      !
      ! Info values
      ! ---
      ! Last iteration proceeded correctly. Continue search
      ! for the minimum.
      !
      integer, parameter :: UNI_CONTINUE = 0
      !
      ! Success: End of iterations. The algorithm has converged to the minimum
      ! with the desired tolerance.
      !
      integer, parameter :: UNI_CONVERGED = 1
      !
      ! Error: Maximum number of iterations exceeded without convergence
      ! with the desired tolerance.
      !
      integer, parameter :: UNI_EXCEEDED_MAX_NITER = 2
      !
      ! Error: The objective function on the given interval,
      ! (MIN_ABSCISSA, MAX_ABSCISSA) has more than
      ! one minimum. 
      !
      integer, parameter :: UNI_MULTIPLE_MINIMA = 3
      
contains

      subroutine uni_firstiter(next_abscissa, x1, f1, x2, f2, x3, f3, max_uncertainty, &
            max_niter, min_abscissa, max_abscissa, pd, info)
            
            real(F64), intent(out)  :: next_abscissa
            real(F64), intent(in)   :: x1
            real(F64), intent(in)   :: f1
            real(F64), intent(in)   :: x2
            real(F64), intent(in)   :: f2
            real(F64), intent(in)   :: x3
            real(F64), intent(in)   :: f3
            real(F64), intent(in)   :: max_uncertainty
            integer, intent(in)            :: max_niter
            real(F64), intent(in)   :: min_abscissa
            real(F64), intent(in)   :: max_abscissa
            type(tunidata), intent(out)    :: pd
            integer, intent(out)           :: info

            logical :: isbracketed
            real(F64) :: uncertainty

            if (.not. (min_abscissa < max_abscissa)) then
                  call msg("UNIOPT: MIN_ABSCISSA MUST BE LOWER THAN MAX_ABSCISSA", MSG_ERROR)
                  stop
            end if

            pd%x(1) = x1
            pd%x(2) = x2
            pd%x(3) = x3
            pd%f(1) = f1
            pd%f(2) = f2
            pd%f(3) = f3
            pd%min_abscissa = min_abscissa
            pd%max_abscissa = max_abscissa
            pd%abstol = max_uncertainty
            pd%max_niter = max_niter
            pd%niter = 0
            pd%lquadratic = .false.
            pd%ipredicted = 0
            pd%iactual = 0
            !
            ! Sort the ordinates in increasing order
            !
            call sortpd(pd)
            isbracketed = test_bracketing(pd%f)
            if (isbracketed) then
                  !
                  ! The minimum is already bracketed.
                  !
                  pd%isbracketed = .true.
                  uncertainty = uni_uncertainty(pd)
                  if (uncertainty < pd%abstol) then
                        info = UNI_CONVERGED
                  else
                        call tightenbracket(next_abscissa, pd%x, pd%f, &
                              pd%lquadratic, pd%ipredicted, pd%iactual)
                        pd%next_abscissa = next_abscissa
                        info = UNI_CONTINUE
                  end if
            else if (pd%f(2) > pd%f(1) .and. pd%f(2) > pd%f(3)) then
                  !
                  ! The objective function has multiple minima. Let the calling
                  ! program decide what should be done.
                  !
                  info = UNI_MULTIPLE_MINIMA
            else
                  !
                  ! The minimum is not bracketed by the initial triplet of points.
                  !
                  pd%isbracketed = .false.
                  uncertainty = uni_uncertainty(pd)
                  if (uncertainty < pd%abstol) then
                        info = UNI_CONVERGED
                  else
                        call expandbracket(next_abscissa, pd%x, pd%f, min_abscissa, max_abscissa)
                        pd%next_abscissa = next_abscissa
                        info = UNI_CONTINUE
                  end if
            end if
      end subroutine uni_firstiter

      
      subroutine uni_nextiter(next_abscissa, fnew, pd, info)
            real(F64), intent(out) :: next_abscissa
            real(F64), intent(in)  :: fnew
            type(tunidata), intent(inout) :: pd
            integer, intent(out)          :: info

            logical :: isbracketed
            real(F64) :: uncertainty

            pd%niter = pd%niter + 1

            if (pd%isbracketed) then
                  if ((pd%next_abscissa > pd%x(2) .and. fnew > pd%f(2) .and. fnew > pd%f(3)) &
                        .or. (pd%next_abscissa < pd%x(2) .and. fnew > pd%f(1) .and. fnew > pd%f(2))) then
                        !
                        ! Return to the calling subprogram is multiple minima
                        ! are found. User should decide what to do next.
                        !
                        info = UNI_MULTIPLE_MINIMA
                  else
                        !
                        ! Generate new triplet of points bracketing the minimum
                        !
                        if (pd%next_abscissa > pd%x(2)) then
                              if (fnew < pd%f(2)) then
                                    pd%x(1) = pd%x(2)
                                    pd%f(1) = pd%f(2)
                                    pd%x(2) = pd%next_abscissa
                                    pd%f(2) = fnew
                                    pd%iactual = 2
                              else
                                    pd%x(3) = pd%next_abscissa
                                    pd%f(3) = fnew
                                    pd%iactual = 3
                              end if
                        else
                              if (fnew < pd%f(2)) then
                                    pd%x(3) = pd%x(2)
                                    pd%f(3) = pd%f(2)
                                    pd%x(2) = pd%next_abscissa
                                    pd%f(2) = fnew
                                    pd%iactual = 2
                              else
                                    pd%x(1) = pd%next_abscissa
                                    pd%f(1) = fnew
                                    pd%iactual = 1
                              end if
                        end if

                        uncertainty = uni_uncertainty(pd)
                        if (uncertainty < pd%abstol) then
                              info = UNI_CONVERGED
                        else
                              call tightenbracket(next_abscissa, pd%x, pd%f, &
                                    pd%lquadratic, pd%ipredicted, pd%iactual)
                              pd%next_abscissa = next_abscissa
                              info = UNI_CONTINUE
                        end if
                  end if
            else
                  !
                  ! The algorithm has been searching for a triplet of points
                  ! bracketing the minimum. Test if it is now bracketed.
                  !
                  if (pd%next_abscissa < pd%x(1)) then
                        !
                        ! The new abscissa is to the left from X(1)
                        !
                        pd%x(3) = pd%x(2)
                        pd%f(3) = pd%f(2)
                        pd%x(2) = pd%x(1)
                        pd%f(2) = pd%f(1)
                        pd%x(1) = pd%next_abscissa
                        pd%f(1) = fnew
                        pd%iactual = 1
                  else
                        !
                        ! The new abscissa is to the right form X(3)
                        !
                        pd%x(1) = pd%x(2)
                        pd%f(1) = pd%f(2)
                        pd%x(2) = pd%x(3)
                        pd%f(2) = pd%f(3)
                        pd%x(3) = pd%next_abscissa
                        pd%f(3) = fnew
                        pd%iactual = 3
                  end if

                  isbracketed = test_bracketing(pd%f)
                  if (isbracketed) then
                        pd%isbracketed = .true.
                        uncertainty = uni_uncertainty(pd)
                        if (uncertainty < pd%abstol) then
                              info = UNI_CONVERGED
                        else
                              call tightenbracket(next_abscissa, pd%x, pd%f, &
                                    pd%lquadratic, pd%ipredicted, pd%iactual)
                              pd%next_abscissa = next_abscissa
                              info = UNI_CONTINUE
                        end if
                  else
                        uncertainty = uni_uncertainty(pd)
                        if (uncertainty < pd%abstol) then
                              !
                              ! The minimum is on the boundary
                              !
                              info = UNI_CONVERGED
                        else
                              !
                              ! The interval where where we search for the minimum
                              ! should be further broadened
                              !
                              call expandbracket(next_abscissa, pd%x, pd%f, &
                                    pd%min_abscissa, pd%max_abscissa)
                              pd%next_abscissa = next_abscissa
                              info = UNI_CONTINUE
                        end if
                  end if
            end if

            if (pd%niter == pd%max_niter .and. info .ne. UNI_CONVERGED) then
                  info = UNI_EXCEEDED_MAX_NITER
            end if
      end subroutine uni_nextiter


      subroutine uni_getsolution(xmin, fmin, uncertainty, pd)
            !
            ! Extract converged solution from the TUNIDATA structure
            !
            real(F64), intent(out) :: xmin
            real(F64), intent(out) :: fmin
            real(F64), intent(out) :: uncertainty
            type(tunidata), intent(in)    :: pd

            integer :: i
            
            i = minloc(pd%f, dim=1)
            fmin = pd%f(i)
            xmin = pd%x(i)
            uncertainty = uni_uncertainty(pd)
      end subroutine uni_getsolution


      function test_bracketing(f)
            !
            ! Test if a given triplet of points is necessarily 
            ! bracketing any minimum
            !
            logical :: test_bracketing
            real(F64), dimension(3), intent(in) :: f

            if (f(2) < f(1) .and. f(2) < f(3)) then
                  test_bracketing = .true.
            else
                  test_bracketing = .false.
            end if
      end function test_bracketing


      function minspacing(f)
            !
            ! Compute minimum allowed spacing between points in the vicinity of 
            ! the objective function's minimum.
            !
            ! Here we take into account the fundamental limitation
            ! on optimization algorithms in a finite-precision arithmetic.
            ! The function F(x) near its minimum has the following Taylor
            ! expansion:
            ! F(x) = F(x0) + 1/2 F''(x0) * (x - x0)**2.
            ! Suppose the distance between F(x0) and the nearest
            ! number in a given finite-precision representation
            ! is eps. Then, the following inequality should be satisfied:
            ! abs(x-x0) > SQRT(2*EPS/F''(x0))
            ! in order to F(x) convey any new information.
            !
            real(F64) :: minspacing
            real(F64), dimension(3), intent(in) :: f

            real(F64) :: fmin

            fmin = minval(f)
            minspacing = sqrt(spacing(fmin))
      end function minspacing


      function uni_uncertainty(unidata)
            !
            ! Compute maximum distance from the exact minimum
            ! of the objective function. This subroutine works
            ! when either the minimum is bracketed or the series
            ! of points is converging to the boundary.
            !
            real(F64)           :: uni_uncertainty
            type(tunidata), intent(in) :: unidata
            
            real(F64) :: a, b
            
            if (unidata%isbracketed) then
                  a = unidata%x(2) - unidata%x(1)
                  b = unidata%x(3) - unidata%x(2)
                  uni_uncertainty = max(a, b)
            else
                  if (unidata%f(1) < unidata%f(2)) then
                        !
                        ! unidata%f(1) < unidata%f(2) < unidata%f(3)
                        !
                        a = unidata%x(2) - unidata%x(1)
                        b = abs(unidata%min_abscissa-unidata%x(1))
                        uni_uncertainty = max(a, b)
                  else
                        !
                        ! unidata%f(3) < unidata%f(2) < unidata%f(1)
                        !
                        a = unidata%x(3) - unidata%x(2)
                        b = abs(unidata%max_abscissa-unidata%x(3))
                        uni_uncertainty = max(a, b)
                  end if
            end if
      end function uni_uncertainty


      subroutine tightenbracket(next_abscissa, x, f, lquadratic, ipredicted, iactual)
            !
            ! Tighten the triplet of points that already brackets the minimum.
            ! Depending on the co-operativity of the objective function, this
            ! algorithm switches between quadratic interpolation and the golden
            ! section technique.
            !
            real(F64), intent(out)              :: next_abscissa
            real(F64), dimension(3), intent(in) :: x
            real(F64), dimension(3), intent(in) :: f
            logical, intent(inout)                     :: lquadratic
            integer, intent(inout)                     :: ipredicted
            integer, intent(in)                        :: iactual

            real(F64) :: eps
            real(F64) :: d1, d2, d3
            real(F64) :: x_quad, x_reflect
            real(F64) :: pred_quad, pred_reflect
            integer :: ipred_reflect, ipred_quad
            logical :: computable
            real(F64) :: a, b
            logical :: quadratic_region

            eps = minspacing(f)
            !
            ! On entry, it is assumed that the quadratic model
            ! is capable of predicting the correct sequence of points.
            ! However, if the previous abscissa has been generated by the 
            ! quadratic model, and the actual sequence of points does
            ! not match the actual one, QUADRATIC_REGION is switched
            ! to .FALSE. 
            !
            quadratic_region = .true.
            if (lquadratic .and. (ipredicted .ne. iactual)) then
                  quadratic_region = .false.
            end if
            !
            ! Calculate the minimum of the quadratic function passing
            ! through three given points
            !
            call quadmin(x_quad, x, f, computable)
            if (computable .and. quadratic_region) then
                  !
                  ! Now follows a modification of original Brent's algorithm.
                  ! We can use quadratic interpolation either to approximate
                  ! the minimum, or to find a point X* above the minimum which
                  ! allows us to braket the minimum more tightly than when using
                  ! orignal Brent's algorithm:
                  !
                  ! ------------------------------------------------> X
                  !   X1                                  X2  X3
                  !                         X*  X_QUAD
                  !
                  !  X_QUAD is the minimum of the parabola.
                  !  F(X1) >> F(X2)
                  !  F(X2) < F(X3)      (Slightly lower value)
                  !  F(X_QUAD) < F(X2)  (Slightly lower value)
                  !  X2 and X3 are close neighbours.
                  !  If we use X_QUAD, then the next bracket will be
                  !  (X1, X_QUAD, X2). It is large. Instead, we reflect
                  !  X3 with respect to X_QUAD, so that (X*, X2, X3)
                  !  is the next bracket, which is much tighter.
                  !  The value of F(X*) should be similar to F(X3) if
                  !  we are close to the true minimum. X* is used 
                  !  only if (X*, X2, X3) is tighter than (X1, X_QUAD, X2).
                  !
                  if (x_quad < x(2)) then
                        pred_quad = x(2) - x(1)
                        x_reflect = TWO * x_quad - x(3)
                        pred_reflect = x(3) - x_reflect
                        ipred_reflect = 1
                  else
                        pred_quad = x(3) - x(2)
                        x_reflect = TWO * x_quad - x(1)
                        pred_reflect = x_reflect - x(1)
                        ipred_reflect = 3
                  end if
                  d1 = abs(x_reflect - x(1))
                  d2 = abs(x_reflect - x(2))
                  d3 = abs(x_reflect - x(3))
                  if (pred_reflect < pred_quad .and. .not. (d1 < eps .or. &
                        d2 < eps .or. d3 < eps .or. x_reflect > x(3) .or. x_reflect < x(1))) then
                        next_abscissa = x_reflect
                        lquadratic = .true.
                        ipredicted = ipred_reflect
                  else
                        ipred_quad = 2
                        d2 = abs(x_quad - x(2))
                        !
                        ! Detect collision with current estimate of
                        ! the minimum. 
                        !
                        if (d2 < eps) then
                              a = x(2) - x(1)
                              b = x(3) - x(2)
                              if (a > b) then
                                    x_quad = x(2) - eps
                                    ipred_quad = 1
                              else
                                    x_quad = x(2) + eps
                                    ipred_quad = 3
                              end if
                        end if
                        d1 = abs(x_quad - x(1))
                        d3 = abs(x_quad - x(3))
                        if (x_quad > x(3) .or. x_quad < x(1) .or. &
                              d1 < eps .or. d3 < eps) then
                              !
                              ! Fall back to golden section if:
                              ! 1) Quadratic model predicts next_abscissa beyond
                              !    the interval bracketing the minimum,
                              ! 2) next_abscissa is too close to another point, thus,
                              !    F(next_abscissa) conveys no new information.
                              ! The third condition is to avoid limiting cycles.
                              !
                              call goldensection(next_abscissa, x)
                              lquadratic = .false.
                        else
                              next_abscissa = x_quad
                              lquadratic = .true.
                              ipredicted = ipred_quad
                        end if
                  end if
            else
                  !
                  ! Fall back to golden section if computing the minimum
                  ! of the parabola is not feasible in finite-precision
                  ! arithmetic
                  !
                  call goldensection(next_abscissa, x)
                  lquadratic = .false.
            end if
      end subroutine tightenbracket


      function worstbracket(xnew, x)
            real(F64)                           :: worstbracket
            real(F64), intent(in)               :: xnew
            real(F64), dimension(3), intent(in) :: x

            real(F64) :: d1, d2

            if (xnew < x(2)) then
                  d1 = abs(x(2) - x(3))
                  d2 = abs(x(3) - xnew)
            else
                  d1 = abs(xnew - x(1))
                  d2 = abs(x(3) - x(2))
            end if
            worstbracket = max(d1, d2)
      end function worstbracket
            
      
      subroutine expandbracket(next_abscissa, x, f, min_abscissa, max_abscissa)
            !
            ! Expand the triplet of points in order to bracket the minimum.
            ! The triplet of points will not be expanded beyond
            ! (MIN_ABSCISSA, MAX_ABSCISSA).
            !
            real(F64), intent(out)              :: next_abscissa
            real(F64), dimension(3), intent(in) :: x
            real(F64), dimension(3), intent(in) :: f
            real(F64), intent(in)               :: min_abscissa
            real(F64), intent(in)               :: max_abscissa

            real(F64) :: xpar, x0
            logical :: computable
            !
            ! There are only two possibilities:
            ! 1. f(1) < f(2) < f(3),
            ! 2. f(1) > f(2) > f(3).
            ! Other possibilities are excluded, because 
            ! the function F(x) is convex.
            !
            call quadmin(xpar, x, f, computable)

            if (f(1) < f(2)) then
                  if (xpar < x(1) .and. xpar > min_abscissa &
                        .and. computable) then
                        x0 = xpar
                  else
                        x0 = x(1)
                  end if
                  next_abscissa = (min_abscissa + x0) / TWO
            else
                  if (xpar > x(3) .and. xpar < max_abscissa &
                        .and. computable) then
                        x0 = xpar
                  else
                        x0 = x(3)
                  end if
                  next_abscissa = (max_abscissa + x0) / TWO
            end if
      end subroutine expandbracket


      subroutine quadmin(xmin, x, f, computable)
            !
            ! Calculate abscissa corresponding to the minimum
            ! of the quadratic function that passes through three given points.
            !
            real(F64), intent(out)              :: xmin
            real(F64), dimension(3), intent(in) :: x
            real(F64), dimension(3), intent(in) :: f
            logical, intent(out)                       :: computable

            real(F64) :: dba, dbc
            real(F64) :: fba, fbc
            real(F64) :: t1, t2
            real(F64) :: w1, w2
            
            dba = x(2) - x(1)
            dbc = x(2) - x(3)
            fba = f(2) - f(1)
            fbc = f(2) - f(3)
            t1 = dba * fbc
            t2 = dbc * fba
            w1 = dba * t1 - dbc * t2
            w2 = t1 - t2
            !
            ! Avoid division by zero (or subnormal numbers).
            ! It could lead to 0/0=NaN or +- Infinity.
            !
            if (abs(w2) > tiny(w2)) then
                  computable = .true.
                  xmin = x(2) - FRAC12 * w1 / w2
            else
                  computable = .false.
                  xmin = ZERO
            end if
      end subroutine quadmin


      subroutine sortpd(pd)
            !
            ! Sort abscissas stored in TUNIDATA structure
            ! in ascending order.
            !
            type(tunidata), intent(inout) :: pd
            
            real(F64), dimension(3) :: f0
            integer, dimension(3) :: ix

            ix = (/1, 2, 3/)
            f0 = pd%f
            call dsort(pd%x, ix, 3)
            pd%f(1) = f0(ix(1))
            pd%f(2) = f0(ix(2))
            pd%f(3) = f0(ix(3))
      end subroutine sortpd


      subroutine goldensection(xnew, x)
            real(F64), intent(out)              :: xnew
            real(F64), dimension(3), intent(in) :: x

            real(F64), parameter :: phi = (ONE + FIVE12) / TWO
            real(F64), parameter :: g = TWO - phi
            real(F64) :: cbint, baint

            cbint = abs(x(3) - x(2))
            baint = abs(x(2) - x(1))

            if (cbint > baint) then
                  xnew = x(2) + g * cbint
            else
                  xnew = x(2) - g * baint
            end if
      end subroutine goldensection


      ! subroutine uni_test()
      !       real(F64), parameter :: min_abscissa = -15d+0
      !       real(F64), parameter :: max_abscissa = 15d+0
      !       real(F64) :: f1, f2, f3
      !       real(F64), parameter :: x1 = -1.2d+0
      !       real(F64), parameter :: x2 = -0.3d+0
      !       real(F64), parameter :: x3 = -0.2d+0
      !       real(F64), parameter :: abstol = 1.d-2
      !       integer :: info
      !       real(F64) :: next_abscissa
      !       real(F64) :: fmin, xmin
      !       real(F64) :: fnew
      !       type(tunidata) :: pd
      !       real(F64) :: uncertainty
            

      !       f1 = f(x1)
      !       f2 = f(x2)
      !       f3 = f(x3)
            
      !       call uni_firstiter(next_abscissa, x1, f1, x2, f2, x3, f3, abstol, &
      !             min_abscissa, max_abscissa, pd, info)
            
      !       do while (info .eq. UNI_CONTINUE)
      !             fnew = f(next_abscissa)
      !             call uni_nextiter(next_abscissa, fnew, pd, info)
      !             print *, "ITERACJA NUMER", pd%niter, "X=",NEXT_ABSCISSA
      !             if (info == UNI_EXCEEDED_MAX_NITER) THEN
      !                   PRINT *, "MAXIMUM NUMBER OF ITERATIONS EXCEEDED"
      !             ELSE IF (INFO == UNI_MULTIPLE_MINIMA) THEN
      !                   PRINT *, "THE OBJECTIVE FUNCTION HAS MULTIPLE MINIMA"
      !             END IF
      !       end do

      !       if (info == UNI_CONVERGED) then
      !             call uni_getsolution(xmin, fmin, uncertainty, pd)
      !             print *, "==================="
      !             print *, "CONVERGED SOLUTION"
      !             PRINT *, "XMIN=",XMIN
      !             PRINT *, "FMIN=",FMIN
      !             PRINT *, "UNCERTAINTY",UNCERTAINTY
      !       end if

      !       contains 
      !             function f(x)
      !                   real(F64) :: f
      !                   real(F64), intent(in) :: x

      !                   f = sin(x)
      !                   !f = x**2
      !                   ! f = 2.d+0*x**4+3.d+0*x**2 + 2 * x - sin(x)
      !                   !f = x**4 + x**2 - cos(x**2)
      !             end function f
      ! end subroutine uni_test
end module uniopt
