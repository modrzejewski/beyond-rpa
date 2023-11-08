! -------------------------------------------------------------------------
! HERMITE-COLOMB INTEGRALS OF ARBITRARY ORDER
! -------------------------------------------------------------------------
! This module provides subroutines for analytic evaluation of
! electron-repulsion integrals in the Hermite-Coulomb basis. The integrals
! can be transformed to the primitive Gaussian-type basis by contracting
! with the E_t^{ij} coefficients. See also the automatically-generated
! version of this module with all loops unrolled.
!
! This module should be used whenever the angular momenta of the integral
! are not known at the compile time or the L value is too large for the
! automatic code to handle. 
! -------------------------------------------------------------------------
! 1. Section 9.9.2 in: Helgaker, T., Jorgensen, P., Olsen, J., Molecular
!    Electronic-Structure Theory, 2000.
!
module hermite
      use gparam
      use math_constants
      use arithmetic

      implicit none
      !
      ! The parameter MAX_EIJ_ORDER is the maximum allowed
      ! order of the Hermite->Gaussian transformation coefficients,
      ! 0 <= I + J <= MAX_EIJ_ORDER.
      ! If you are to modify the parameter MAX_EIJ_ORDER, take into account that:
      ! 1. The parameter MAX_EIJ_ORDER depends on the maximum Cartesian 
      !    angular momentum of a product of two Gaussian-type functions.
      ! 2. The coefficients EIJ are used to compute the single-electron 
      !    kinetic-energy integrals, that is, single-electron overlap 
      !    integrals with one of the momenta raised by 2 are needed.
      ! 3. The coefficients EIJ may be used when computing derivatives,
      !    which leads to Gaussian-type functions with the angular momentum
      !    raised according to the maximum order of the derivative.
      !
      integer, parameter :: MAX_EIJ_ORDER = 2 * MAX_L + 2
      integer, parameter :: MAX_CHIDX = 4 * MAX_L + 1
      integer, private :: k
      integer, dimension(0:MAX_EIJ_ORDER), parameter :: &
            EIJPOS_LOOKUP = [((k * (2 * k**2 + 3 * k + 1)) / 6, k = 0, MAX_EIJ_ORDER)]
      real(F64), dimension(0:max(MAX_CHIDX, MAX_EIJ_ORDER)), private, parameter :: &
            HER_DBLEINT = [(dble(k), k = 0, max(MAX_CHIDX, MAX_EIJ_ORDER))]

contains

      pure function hposition(l, j, t)
            !
            ! Calculate the position of the E^{ij}_t element
            ! (i + j = l) in a linearized array
            !
            integer :: hposition

            integer, intent(in) :: l, j, t

            hposition = j * (l + 1) + t + 1
      end function hposition


      pure function eijmatrix_position(l, j, t)
            integer :: eijmatrix_position

            integer, intent(in) :: l, j, t

            eijmatrix_position = eijpos_lookup(l) + j * (l + 1) + t + 1
      end function eijmatrix_position


      pure subroutine eijmatrix_new_level(oldlev, newlev, lold, p, xpa, xpb)
            ! ---------------------------------------------------------------
            ! Compute the coefficients of the transformation from a product
            ! of Gassian-type functions to Hermite-Gaussian basis.
            ! The E^{ij}_t coefficients are defined in Helgaker's textbook,
            ! see Eqs 9.5.6 and 9.5.7 in [1]. This subroutine performs a 
            ! single iteration of the vertical recurrence relation.
            ! ---------------------------------------------------------------
            ! 1. Helgaker, T., Jorgensen, P., Olsen, J., Molecular
            !    Electronic-Structure Theory, 2000.
            !
            real(F64), dimension(:), intent(in)  :: oldlev
            real(F64), dimension(:), intent(out) :: newlev
            integer, intent(in)                  :: lold
            real(F64), intent(in)                :: p
            real(F64), intent(in)                :: xpa
            real(F64), intent(in)                :: xpb

            integer :: lnew
            integer :: k, t, u, pos1, pos2
            real(F64) :: e1, e2
            real(F64) :: twop
            real(F64) :: dt, di, dk

            lnew = lold + 1
            u = 1
            twop = two * p

            !
            ! E^{lnew, 0}_t, t = 0, ... , lnew
            ! ---
            ! E^{lnew, 0}_0
            !
            pos1 = 1
            pos2 = 2

            e1 = oldlev(pos1)
            e2 = oldlev(pos2)
            newlev(u) = xpa * e1 + e2
            u = u + 1
            !
            ! E^{lnew, 0}_t, t = 1, ... , lnew
            !
            di = HER_DBLEINT(lnew)
            pos1 = 1
            do t = 1, lnew
                  dt = HER_DBLEINT(t)
                  e1 = oldlev(pos1)
                  newlev(u) = one / (twop * dt) * di * e1
                  u = u + 1
                  pos1 = pos1 + 1
            end do
            !
            ! E^{lnew - k, k}_t, k = 1, ... , lnew - 1
            !
            do k = 1, lnew - 1
                  !
                  ! E^{lnew - k, k}_0
                  !
                  di = HER_DBLEINT(lnew-k)
                  dk = HER_DBLEINT(k)
                  pos1 = hposition(lold, k, 0)
                  pos2 = pos1 + 1
                  e1 = oldlev(pos1)
                  e2 = oldlev(pos2)
                  newlev(u) = xpa * e1 + e2
                  u = u + 1
                  !
                  ! E^{lnew - k, k}_t, t = 1, ... , lnew
                  !
                  pos2 = hposition(lold, k - 1, 0)
                  do t = 1, lnew
                        dt = HER_DBLEINT(t)
                        e1 = oldlev(pos1)
                        e2 = oldlev(pos2)
                        newlev(u) = one / (twop * dt) * (di * e1 + dk * e2)
                        u = u + 1
                        pos1 = pos1 + 1
                        pos2 = pos2 + 1
                  end do
            end do
            !
            ! E^{0, lnew}_0
            !
            pos1 = hposition(lold, lold, 0)
            pos2 = pos1 + 1
            e1 = oldlev(pos1)
            e2 = oldlev(pos2)
            newlev(u) = xpb * e1 + e2
            u = u + 1
            !
            ! E^{0, lnew}_t, t = 1, ... , lnew
            !
            dk = HER_DBLEINT(lnew)
            pos1 = hposition(lold, lnew - 1, 0)
            do t = 1, lnew
                  dt = HER_DBLEINT(t)
                  e1 = oldlev(pos1)
                  newlev(u) = one / (twop * dt) * dk * e1
                  u = u + 1
                  pos1 = pos1 + 1
            end do
      end subroutine eijmatrix_new_level


      pure subroutine eijmatrix(l, seed, p, xpa, xpb, eijm)
            ! ---------------------------------------------------------------
            ! Compute the coefficients of the transformation from a product
            ! of Gassian-type functions to Hermite-Gaussian basis.
            ! The E^{ij}_t coefficients are defined in Helgaker's textbook,
            ! see Eqs 9.5.6 and 9.5.7 in [1].
            ! ---------------------------------------------------------------
            ! 1. Helgaker, T., Jorgensen, P., Olsen, J., Molecular
            !    Electronic-Structure Theory, 2000.
            !
            integer, intent(in)                  :: l
            real(F64), intent(in)                :: seed
            real(F64), intent(in)                :: p
            real(F64), intent(in)                :: xpa
            real(F64), intent(in)                :: xpb
            real(F64), dimension(:), intent(out) :: eijm

            integer :: new_level, old_level, k
            real(F64), dimension(2) :: init_array

            eijm(1) = seed

            if (l > 0) then
                  init_array(1) = seed
                  init_array(2) = ZERO
                  new_level = 2
                  call eijmatrix_new_level(init_array, eijm(new_level:), 0, p, xpa, xpb)
                  old_level = new_level
                  new_level = new_level + 4
                  do k = 2, l
                        call eijmatrix_new_level(eijm(old_level:), eijm(new_level:), k - 1, p, xpa, xpb)
                        old_level = new_level
                        new_level = new_level + (k + 1)**2
                  end do
            end if
      end subroutine eijmatrix


      subroutine chints_new_level(oldlev, newlev, order, xpc, ypc, zpc)
            real(F64), dimension(:, :, :), intent(in)  :: oldlev
            real(F64), dimension(:, :, :), intent(out) :: newlev
            integer, intent(in)                        :: order
            real(F64), intent(in)                      :: xpc
            real(F64), intent(in)                      :: ypc
            real(F64), intent(in)                      :: zpc

            call vloop()
            
            contains

                  subroutine tloop(u, v)
                        integer, intent(in) :: u, v
                        integer :: t
                        !
                        ! T == 1
                        !
                        newlev(2, u, v) = xpc * oldlev(1, u, v)
                        !
                        ! T > 1
                        !
                        do t  = 2, order - u - v + 2
                              newlev(t + 1, u, v) = HER_DBLEINT(t-1) * oldlev(t - 1, u, v) + xpc * oldlev(t, u, v)
                        end do
                  end subroutine tloop

                  subroutine uloop(v)
                        integer, intent(in) :: v
                        integer :: u
                        !
                        ! U == 1
                        !
                        newlev(1, 2, v) = ypc * oldlev(1, 1, v)
                        call tloop(1, v)
                        !
                        ! U > 1
                        !
                        do u = 2, order - v + 1
                              newlev(1, u + 1, v) = HER_DBLEINT(u-1) * oldlev(1, u - 1, v) + ypc * oldlev(1, u, v)
                              call tloop(u, v)
                        end do
                  end subroutine uloop

                  subroutine vloop()
                        integer :: v
                        !
                        ! V == 1
                        !
                        newlev(1, 1, 2) = zpc * oldlev(1, 1, 1)
                        call uloop(1)
                        !
                        ! V > 1
                        !
                        do v = 2, order
                              newlev(1, 1, v + 1) = HER_DBLEINT(v-1) * oldlev(1, 1, v - 1) + zpc * oldlev(1, 1, v)
                              call uloop(v)
                        end do
                  end subroutine vloop
      end subroutine chints_new_level


      subroutine chints(l, fm, rtuv, xpc, ypc, zpc, p)
            ! -----------------------------------------------------------------------------
            ! Calculate Hermite-Coulomb integrals for the McMurchie-Davidson
            ! ERI algorithm. A Hermite-Coulomb integral of artibrary order
            ! can be computed by calling this subroutine. See also 
            ! the automatically generated code, which is optimized for the integrals
            ! for which the L value is known at the compile time.
            ! -----------------------------------------------------------------------------
            ! 1. Section 9.9.2 (electron-repulsion integrals in Hermite-Coulomb basis) in
            !    T. Helgaker, Molecular Electronic-Structure Theory
            ! -----------------------------------------------------------------------------
            ! L    - The maximum order of the Hermite-Coulomb integral R_{tuv}:
            !        0 <= t+u+v <= L. If the Hermite-Coulomb integrals are used to 
            !        computed non-differentiated electron-repulsion integrals, then the L
            !        value should be equal to the sum of the orbitals' angular momenta.
            !        The maximum allowed value of the L parameter is MAX_CHIDX-1.
            !
            ! FM   - Array of values of the Boys function,
            !        where FM(1) = F_0(x), FM(2) = F_1(x), etc.
            !
            ! RTUV - On exit, the RTUV array contains Hermite Coulomb integrals:
            !        R_{tuv} := R_{tuv}^0 = RTUV(t+1, u+1, v+1).
            !        The Hermite-Coulomb integrals are calculated up to L-th order, that
            !        is 0 <= t+u+v <= L. Note that the indices in the RTUV matrix start
            !        from 1, not 0. The RTUV integral is defined as in Eq. 9.9.13 in [1],
            !        where the upper index N is set to 0.
            !
            ! XPC, - Input, the components of the R_{PC} vector defined in 
            ! YPC,   Helgaker's textbook
            ! XPC
            !
            ! P    - Input, see the definition of p and \alpha in Helgaker's
            !        textbook
            !
            integer, intent(in)                                                :: l
            real(F64), dimension(MAX_CHIDX), intent(in)                        :: fm
            real(F64), dimension(MAX_CHIDX, MAX_CHIDX, MAX_CHIDX), intent(out) :: rtuv
            real(F64), intent(in)                                              :: xpc
            real(F64), intent(in)                                              :: ypc
            real(F64), intent(in)                                              :: zpc
            real(F64), intent(in)                                              :: p

            real(F64), dimension(MAX_CHIDX, MAX_CHIDX, MAX_CHIDX) :: work1
            integer :: k
            !
            ! Eq. 9.9.14 in [1]:
            ! R_{000}^l(p, R_{PC}) = (-2p)^l F_l(p R_{PC}^2).
            !
            work1(1, 1, 1) = (-TWO * p)**l * fm(l + 1)
            !
            ! Now, the upper index of the Hermite-Coulomb R_{tuv}^n integrals
            ! will be lowered by applying the recurrence relations,
            ! Eqs 9.9.18-20 in [1].
            !
            do k = 1, l-1, 2
                  call chints_new_level(work1, rtuv, k, xpc, ypc, zpc)
                  rtuv(1, 1, 1) = (-TWO * p)**(l-k) *  fm(l + 1 - k)
                  call chints_new_level(rtuv, work1, k+1, xpc, ypc, zpc)
                  work1(1, 1, 1) = (-TWO * p)**(l-k-1) *  fm(l - k)
            end do

            if (modulo(l, 2) == 0) then
                  rtuv = work1
            else
                  call chints_new_level(work1, rtuv, l, xpc, ypc, zpc)
                  rtuv(1, 1, 1) = fm(1)
            end if
      end subroutine chints
end module hermite
