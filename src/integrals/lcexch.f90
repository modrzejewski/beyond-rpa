module lcexch
      use math_constants
      use arithmetic
      
      implicit none
      save 
      
      real(F64), private :: LCE_OMEGA
      real(F64), private :: LCE_KAPPA = ZERO
      logical, private   :: LCE_ISLCEXCH = .false.
      logical, private   :: LCE_ISSREXX = .false.
      real(F64), private :: LCE_SREXXSCAL = ZERO
      !
      ! Portion of long-range exact exchange
      !
      real(F64), parameter, private :: LCE_LREXXSCAL = one

contains
      
      subroutine lce_setuplcfunc(omega, srexxscal)
            real(F64), intent(in) :: omega
            real(F64), optional, intent(in) :: srexxscal

            real(F64), parameter :: eps = 1.0E-12_F64

            LCE_ISLCEXCH = .true.
            LCE_OMEGA = omega
            LCE_KAPPA = ZERO

            if (present(srexxscal)) then
                  if (abs(srexxscal) > eps) then
                        LCE_ISSREXX = .true.
                        LCE_SREXXSCAL = srexxscal
                  else
                        LCE_ISSREXX = .false.
                        LCE_SREXXSCAL = ZERO
                  end if
            else
                  LCE_SREXXSCAL = ZERO
                  LCE_ISSREXX = .false.
            end if
      end subroutine lce_setuplcfunc


      subroutine lce_unsetlcfunc()
            !
            ! Reset to default values
            !
            LCE_ISLCEXCH = .false.
            LCE_ISSREXX = .false.
            LCE_SREXXSCAL = ZERO
      end subroutine lce_unsetlcfunc


      subroutine lce_activate()
            !
            ! Set range-separation parameter LCE_OMEGA:
            ! ERF(LCE_OMEGA * R_{12}) function controls
            ! the range-separation of the exchange hole.
            !
            if (LCE_ISLCEXCH) then
                  LCE_KAPPA = one / LCE_OMEGA**2
            end if
      end subroutine lce_activate


      subroutine lce_deactivate()
            LCE_KAPPA = ZERO
      end subroutine lce_deactivate


      pure function lce_alpha(p, q)
            ! ----------------------------------------
            ! Alpha factor which scales argument 
            ! of the Boys function:
            ! ALPHA = P * Q / (P + Q + KAPPA * P * Q)
            ! KAPPA == 0 if integrals are evaluated
            ! for full 1/R_{12} operator.
            ! ----------------------------------------
            ! 1. Gill, P.M.W., Adamson, R.D., A family
            !    of attenuated Coulomb operators,
            !    Chem. Phys. Lett. 261, 105(1996)
            !
            real(F64) :: lce_alpha
            real(F64), intent(in) :: p
            real(F64), intent(in) :: q

            real(F64) :: pq
            !
            ! LCE_KAPPA = 1 / LCE_OMEGA**2
            !
            pq = p * q
            lce_alpha = pq / (p + q + LCE_KAPPA * pq)
      end function lce_alpha


      pure function lce_normfactor(p, q)
            ! ------------------------------------------
            ! Normalizing factor in front of Gaussian
            ! density distributions:
            ! 2 * PI^(5/2) / (P * Q * 
            ! SQRT(P + Q + KAPPA * P * Q))
            ! KAPPA == 0 if integrals are evaluated for 
            ! full 1/R_{12} operator.
            ! ------------------------------------------
            ! 1. Gill, P.M.W., Adamson, R.D., A family
            !    of attenuated Coulomb operators,
            !    Chem. Phys. Lett. 261, 105(1996)
            !
            real(F64) :: lce_normfactor
            real(F64), intent(in) :: p
            real(F64), intent(in) :: q

            real(F64) :: sq, pq

            pq = p * q
            sq = sqrt(p + q + LCE_KAPPA * pq)
            lce_normfactor = two * pi52 / (pq * sq)
      end function lce_normfactor
end module lcexch
